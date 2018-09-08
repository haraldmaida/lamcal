//! [Church Booleans] and logical operations
//!
//! Church encoding for representing:
//!
//! * `True`
//! * `False`
//! * `not`
//! * `and`
//! * `or`
//! * `xor`
//! * `beq`
//! * `if_else`
//!
//! [Church Booleans]: https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans

use std::collections::HashSet;

use environment::Binding;
use term::{app, lam, var, Term};

/// Creates a set of bindings for all data types, data structures and operators
/// implemented in this module.
pub fn default_bindings() -> HashSet<Binding> {
    binds! {
        True => True(),
        False => False(),
        NOT => not(),
        not => not(),
        AND => and(),
        and => and(),
        OR => or(),
        or => or(),
        XOR => xor(),
        xor => xor(),
        BEQ => beq(),
        beq => beq(),
        IfElse => if_else(),
        if_else => if_else(),
    }
}

/// Boolean : True
///
/// True ≡ λab.a
#[allow(non_snake_case)]
pub fn True() -> Term {
    lam("a", lam("b", var("a")))
}

/// Boolean : False
///
/// False ≡ λab.b
#[allow(non_snake_case)]
pub fn False() -> Term {
    lam("a", lam("b", var("b")))
}

impl From<bool> for Term {
    /// Converts a value of type `bool` into the Church encoding of its value.
    ///
    /// # Examples
    ///
    /// ```
    /// use lamcal::church_encoded::boolean::{False, True};
    /// use lamcal::{lam, var, Term};
    ///
    /// let term: Term = true.into();
    ///
    /// assert_eq!(term, True());
    /// assert_eq!(Term::from(false), False());
    ///
    /// let term: Term = false.into();
    ///
    /// assert_eq!(Term::from(true), lam("a", lam("b", var("a"))));
    /// assert_eq!(term, lam("a", lam("b", var("b"))));
    /// ```
    fn from(val: bool) -> Self {
        if val {
            True()
        } else {
            False()
        }
    }
}

/// NOT : negation
///
/// NOT ≡ λp.p False True
///
/// # Examples
///
/// ```
/// use lamcal::church_encoded::boolean::{not, False, True};
/// use lamcal::{app, reduce, Enumerate, NormalOrder};
///
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app(not(), True())),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app(not(), False())),
///     True()
/// );
/// ```
pub fn not() -> Term {
    lam("p", app![var("p"), False(), True()])
}

/// AND : conjunction
///
/// AND ≡ λpq.pqp
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{and, False, True};
/// use lamcal::{reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![and(), True(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![and(), True(), False()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![and(), False(), True()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![and(), False(), False()]),
///     False()
/// );
/// # }
/// ```
pub fn and() -> Term {
    lam("p", lam("q", app![var("p"), var("q"), var("p")]))
}

/// OR : disjunction
///
/// OR ≡ λpq.ppq
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{or, False, True};
/// use lamcal::{reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![or(), True(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![or(), True(), False()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![or(), False(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![or(), False(), False()]),
///     False()
/// );
/// # }
/// ```
pub fn or() -> Term {
    lam("p", lam("q", app![var("p"), var("p"), var("q")]))
}

/// XOR : exclusive disjunction
///
/// XOR ≡ λpq.p (NOT q) q
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{xor, False, True};
/// use lamcal::{app, reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![xor(), True(), True()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![xor(), True(), False()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![xor(), False(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![xor(), False(), False()]),
///     False()
/// );
/// # }
/// ```
pub fn xor() -> Term {
    lam(
        "p",
        lam("q", app![var("p"), app(not(), var("q")), var("q")]),
    )
}

/// BEQ : boolean equality
///
/// BEQ ≡ λpq.p q (NOT q)
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{beq, False, True};
/// use lamcal::{app, reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![beq(), True(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![beq(), True(), False()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![beq(), False(), True()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![beq(), False(), False()]),
///     True()
/// );
/// # }
/// ```
pub fn beq() -> Term {
    lam(
        "p",
        lam("q", app![var("p"), var("q"), app(not(), var("q"))]),
    )
}

/// IF_ELSE : boolean equality
///
/// IF_ELSE ≡ λpab.p a b
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{if_else, False, True};
/// use lamcal::{app, reduce, var, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![if_else(), True(), var("x"), var("y")]),
///     var("x")
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![if_else(), False(), var("x"), var("y")]),
///     var("y")
/// );
/// # }
/// ```
pub fn if_else() -> Term {
    lam("p", lam("a", lam("b", app![var("p"), var("a"), var("b")])))
}
