//! [Church Booleans] and logical operations
//!
//! Church encoding for representing:
//!
//! * `True`
//! * `False`
//! * `Not`
//! * `And`
//! * `Or`
//! * `Xor`
//! * `Beq`
//! * `If_Else`
//!
//! [Church Booleans]: https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans

#![allow(non_snake_case)]

use std::collections::HashSet;

use environment::Binding;
use term::{app, lam, var, Term};

/// Creates a set of bindings for all data types, data structures and operators
/// implemented in this module.
pub fn default_bindings() -> HashSet<Binding> {
    binds! {
        True => True(),
        False => False(),
        Not => Not(),
        And => And(),
        or => or(),
        Xor => Xor(),
        Beq => Beq(),
        If_Else => If_Else(),
    }
}

/// Boolean : True
///
/// True ≡ λab.a
pub fn True() -> Term {
    lam("a", lam("b", var("a")))
}

/// Boolean : False
///
/// False ≡ λab.b
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
/// Not ≡ λp.p False True
///
/// # Examples
///
/// ```
/// use lamcal::church_encoded::boolean::{False, Not, True};
/// use lamcal::{app, reduce, Enumerate, NormalOrder};
///
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app(Not(), True())),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app(Not(), False())),
///     True()
/// );
/// ```
pub fn Not() -> Term {
    lam("p", app![var("p"), False(), True()])
}

/// AND : conjunction
///
/// And ≡ λpq.pqp
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{And, False, True};
/// use lamcal::{reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![And(), True(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![And(), True(), False()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![And(), False(), True()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![And(), False(), False()]),
///     False()
/// );
/// # }
/// ```
pub fn And() -> Term {
    lam("p", lam("q", app![var("p"), var("q"), var("p")]))
}

/// OR : disjunction
///
/// or ≡ λpq.ppq
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
/// Xor ≡ λpq.p (NOT q) q
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// use lamcal::church_encoded::boolean::{False, True, Xor};
/// use lamcal::{app, reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Xor(), True(), True()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Xor(), True(), False()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Xor(), False(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Xor(), False(), False()]),
///     False()
/// );
/// # }
/// ```
pub fn Xor() -> Term {
    lam(
        "p",
        lam("q", app![var("p"), app(Not(), var("q")), var("q")]),
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
/// use lamcal::church_encoded::boolean::{Beq, False, True};
/// use lamcal::{app, reduce, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Beq(), True(), True()]),
///     True()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Beq(), True(), False()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Beq(), False(), True()]),
///     False()
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![Beq(), False(), False()]),
///     True()
/// );
/// # }
/// ```
pub fn Beq() -> Term {
    lam(
        "p",
        lam("q", app![var("p"), var("q"), app(Not(), var("q"))]),
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
/// use lamcal::church_encoded::boolean::{False, If_Else, True};
/// use lamcal::{app, reduce, var, Enumerate, NormalOrder};
///
/// # fn main() {
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![If_Else(), True(), var("x"), var("y")]),
///     var("x")
/// );
/// assert_eq!(
///     reduce::<NormalOrder<Enumerate>>(&app![If_Else(), False(), var("x"), var("y")]),
///     var("y")
/// );
/// # }
/// ```
pub fn If_Else() -> Term {
    lam("p", lam("a", lam("b", app![var("p"), var("a"), var("b")])))
}
