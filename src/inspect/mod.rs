//! The inspection mechanism for tracing and interrupting reduction and
//! evaluation functions.
//!
//! The inspection mechanism allows to trace the steps of reduction and
//! evaluation functions and to stop reduction or evaluation depending on some
//! condition.
//!
//! The central piece of the inspection mechanism is the
//! [`Inspect`](trait.Inspect.html) trait. The `reduce_inspected` and
//! `evaluate_inspected` functions take an implementation of the `Inspect` trait
//! to extend the processing with some additional functionality. An
//! implementation of the `Inspect` trait is called an inspection.
//!
//! This module provides some implementation of the `Inspect` trait:
//!
//! * [Limit](struct.Limit.html) : Limits the number of steps executed during
//!   evaluation or reduction.
//! * [Trace](struct.Limit.html) : Allows to log or print the results of each
//!   step during evaluation or reduction.
//! * [Collect](struct.Collect.html) : Collects clones of the term resulting
//!   from each single step during evaluation or reduction.
//!
//! Inspections can be combined using the combinator inspections:
//!
//! * [And](struct.And.html) : Combines two inspections into one inspection that
//!   stops processing if both inspections return `Stop::Yes`.
//! * [Or](struct.Or.html) : Combines two inspections into one inspection that
//!   stops processing if one of them returns `Stop::Yes`.
//!
//! To construct the combinator inspections in a more concise syntax the
//! functions [`and`](fn.and.html) and [`or`](fn.or.html) are provided.

use std::iter::IntoIterator;
use std::ops::{BitAnd, BitOr};
use std::vec;

use term::Term;

/// The inspect trait allows to inspect the current term before each step
/// during processing of a term and conditionally stop further processing.
///
/// This trait can be used to trace the terms before each step during processing
/// or to limit the number of steps being executed on some condition like
/// the number of steps performed so far.
///
/// The inspect function can hold some state and modify it according to the
/// implemented logic.
pub trait Inspect {
    /// Inspects the term before the next step during processing and returns
    /// whether to continue processing or to stop it.
    ///
    /// If `Stop::Yes` is returned the processing will stop immediately without
    /// performing the next step.
    fn inspect(&mut self, term: &Term) -> Stop;
}

/// Indicator whether to stop or continue some processing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stop {
    /// Stop processing
    Yes,
    /// Continue processing
    No,
}

impl BitAnd for Stop {
    type Output = Self;

    fn bitand(self, rhs: Self) -> <Self as BitAnd<Self>>::Output {
        use self::Stop::*;
        match (self, rhs) {
            (Yes, Yes) => Yes,
            _ => No,
        }
    }
}

impl BitOr for Stop {
    type Output = Self;

    fn bitor(self, rhs: Self) -> <Self as BitOr<Self>>::Output {
        use self::Stop::*;
        match (self, rhs) {
            (No, No) => No,
            _ => Yes,
        }
    }
}

/// Combines two given inspections into an `And` combinator inspection.
///
/// An inspection is any implementation of the `Inspect` trait.
pub fn and<LHS, RHS>(lhs: LHS, rhs: RHS) -> And<LHS, RHS> {
    And::new(lhs, rhs)
}

/// AND combinator for two inspections that returns `Stop::Yes` if both
/// inspections return `Stop::Yes`.
///
/// An inspection is any implementation of the `Inspect` trait.
///
/// The combinator always calls both inspections even if the first inspection
/// results in a stop indicator that will not change due to the result of the
/// second inspection.
#[allow(missing_debug_implementations)]
pub struct And<LHS, RHS> {
    lhs: LHS,
    rhs: RHS,
}

impl<LHS, RHS> And<LHS, RHS> {
    /// Constructs a new `And` combinator inspection out of the two given
    /// inspections.
    pub fn new(lhs: LHS, rhs: RHS) -> Self {
        And { lhs, rhs }
    }

    /// Deconstructs this inspection into its two containing inspections.
    pub fn unwrap(self) -> (LHS, RHS) {
        (self.lhs, self.rhs)
    }

    /// Returns a reference to the left inspection.
    pub fn left(&self) -> &LHS {
        &self.lhs
    }

    /// Returns a reference to the right inspection.
    pub fn right(&self) -> &RHS {
        &self.rhs
    }
}

impl<LHS, RHS> Inspect for And<LHS, RHS>
where
    LHS: Inspect,
    RHS: Inspect,
{
    fn inspect(&mut self, term: &Term) -> Stop {
        self.lhs.inspect(term) & self.rhs.inspect(term)
    }
}

/// Combines the two given inspections into an `Or` combinator inspection.
///
/// An inspection is any implementation of the `Inspect` trait.
///
/// # Examples
///
/// ```
/// use lamcal::inspect::{or, Collect, Limit};
/// use lamcal::{app, lam, var, Enumerate, NormalOrder};
///
/// let mut term = app(
///     lam("x", app(lam("y", app(var("x"), var("y"))), var("z"))),
///     var("a"),
/// );
///
/// let mut inspection = or(Limit::new(42), Collect::new());
///
/// term.reduce_inspected::<NormalOrder<Enumerate>, _>(&mut inspection);
///
/// assert_eq!(inspection.left().count(), 2);
/// assert_eq!(
///     inspection.right().terms(),
///     &[
///         app(
///             lam("x", app(lam("y", app(var("x"), var("y"))), var("z"))),
///             var("a")
///         ),
///         app(lam("x", app(var("x"), var("z"))), var("a")),
///     ]
/// );
/// assert_eq!(term, app(var("a"), var("z")));
/// ```
pub fn or<LHS, RHS>(lhs: LHS, rhs: RHS) -> Or<LHS, RHS> {
    Or::new(lhs, rhs)
}

/// OR combinator for two inspections that returns `Stop::Yes` if one of the
/// inspections returns `Stop::Yes`.
///
/// An inspection is any implementation of the `Inspect` trait.
///
/// The combinator always calls both inspections even if the first inspection
/// results in a stop indicator that will not change due to the result of the
/// second inspection.
#[allow(missing_debug_implementations)]
pub struct Or<LHS, RHS> {
    lhs: LHS,
    rhs: RHS,
}

impl<LHS, RHS> Or<LHS, RHS> {
    /// Constructs a new `Or` combinator inspection out of the two given
    /// inspections.
    pub fn new(lhs: LHS, rhs: RHS) -> Self {
        Or { lhs, rhs }
    }

    /// Deconstructs this inspection into its two containing inspections.
    pub fn unwrap(self) -> (LHS, RHS) {
        (self.lhs, self.rhs)
    }

    /// Returns a reference to the left inspection.
    pub fn left(&self) -> &LHS {
        &self.lhs
    }

    /// Returns a reference to the right inspection.
    pub fn right(&self) -> &RHS {
        &self.rhs
    }
}

impl<LHS, RHS> Inspect for Or<LHS, RHS>
where
    LHS: Inspect,
    RHS: Inspect,
{
    fn inspect(&mut self, term: &Term) -> Stop {
        self.lhs.inspect(term) | self.rhs.inspect(term)
    }
}

/// No operation inspection.
///
/// An inspection that does nothing and always returns to don't stop thus
/// continue with processing.
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone, PartialEq)]
pub struct NoOp;

impl Inspect for NoOp {
    fn inspect(&mut self, _term: &Term) -> Stop {
        Stop::No
    }
}

/// Limits the number of steps a reduction or evaluation function performs
/// until processing is stopped.
///
/// This inspection counts each call to the `Inspect::inspect` function. If the
/// number of calls counted so far is less than the limit for which the `Limit`
/// has been constructed it returns `Stop::No` to continue processing. If the
/// limit has been reached it returns `Stop::Yes` to instruct the evaluation
/// or reduction function to stop processing.
///
/// A limit of 0 means that not reduction will be performed at all.
///
/// # Examples
///
/// ```
/// # use lamcal::{app, lam, var, Enumerate, NormalOrder};
/// # use lamcal::inspect::Limit;
/// let mut term = app(
///     lam("a", app(var("a"), var("a"))),
///     lam("a", app(var("a"), var("a"))),
/// );
///
/// term.reduce_inspected::<NormalOrder<Enumerate>, _>(&mut Limit::new(5));
/// ```
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone, PartialEq)]
pub struct Limit {
    limit: u32,
    count: u32,
}

impl Limit {
    /// Constructs a new `Limit` inspection with the given number of steps as
    /// the limit of reduction or evaluation steps.
    pub fn new(limit: u32) -> Self {
        Limit { limit, count: 0 }
    }

    /// The limit for the number steps.
    pub fn limit(&self) -> u32 {
        self.limit
    }

    /// The number of steps counted so far.
    pub fn count(&self) -> u32 {
        self.count
    }
}

impl Default for Limit {
    fn default() -> Self {
        Limit::new(::std::u32::MAX)
    }
}

impl Inspect for Limit {
    fn inspect(&mut self, _term: &Term) -> Stop {
        if self.count < self.limit {
            self.count += 1;
            Stop::No
        } else {
            Stop::Yes
        }
    }
}

/// Trace turns a closure into an implementation of the `Inspect` trait.
///
/// Trace is an implementation of `Inspect` that takes a closure to be called
/// before each reduction or evaluation step. It always returns `Stop::No`.
///
/// # Examples
///
/// ```
/// # use lamcal::{app, lam, var, Enumerate, NormalOrder};
/// # use lamcal::inspect::Trace;
/// let mut term = app(
///     lam("x", app(lam("y", app(var("x"), var("y"))), var("z"))),
///     var("a"),
/// );
///
/// term.reduce_inspected::<NormalOrder<Enumerate>, _>(&mut Trace::new(|to_reduce| {
///     println!("reducing: {}", to_reduce);
/// }));
///
/// assert_eq!(term, app(var("a"), var("z")));
/// ```
#[allow(missing_debug_implementations)]
pub struct Trace<Log> {
    log: Log,
}

impl<Log> Trace<Log>
where
    Log: Fn(&Term),
{
    /// Constructs a new `Trace` inspection from the given closure.
    pub fn new(log: Log) -> Self {
        Trace { log }
    }

    /// Returns a reference to the closure of this trace inspection.
    pub fn log(&self) -> &Log {
        &self.log
    }
}

impl<Log> Inspect for Trace<Log>
where
    Log: Fn(&Term),
{
    fn inspect(&mut self, term: &Term) -> Stop {
        (self.log)(term);
        Stop::No
    }
}

/// Collects clones of the terms representing the results of each step during
/// reduction or evaluation of a term.
#[derive(Debug, Clone, PartialEq)]
pub struct Collect {
    terms: Vec<Term>,
}

impl Collect {
    /// Constructs a new `Collect` inspection.
    pub fn new() -> Self {
        Collect { terms: Vec::new() }
    }

    /// Unwraps the underlying vector of collected terms.
    pub fn unwrap(self) -> Vec<Term> {
        self.terms
    }

    /// Returns a reference to a slice of collected terms.
    pub fn terms(&self) -> &[Term] {
        &self.terms
    }
}

impl Default for Collect {
    fn default() -> Self {
        Collect { terms: Vec::new() }
    }
}

impl IntoIterator for Collect {
    type Item = Term;
    type IntoIter = vec::IntoIter<Term>;

    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.terms.into_iter()
    }
}

impl Inspect for Collect {
    fn inspect(&mut self, term: &Term) -> Stop {
        self.terms.push(term.clone());
        Stop::No
    }
}

#[cfg(test)]
mod tests;
