//! [Church numerals] and arithmetic operations
//!
//! Church encoding of natural numbers and arithmetic operations.
//!
//! * `zero`
//! * `one`
//! * `succ`
//!
//! [Church numerals]: https://en.wikipedia.org/wiki/Church_encoding#Church_numerals

use std::collections::HashSet;

use environment::Binding;
use term::{app, lam, var, Term};

/// Creates a set of bindings for all data types, data structures and operators
/// implemented in this module.
pub fn default_bindings() -> HashSet<Binding> {
    binds! {
        zero => zero(),
        one => one(),
        succ => succ(),
    }
}

/// Number : 0 : apply f no times to a
///
/// ZERO ≡ λfa.a
pub fn zero() -> Term {
    lam("f", lam("a", var("a")))
}

/// Number : 1 : apply f once to a
///
/// ONE ≡ λfa.fa
pub fn one() -> Term {
    lam("f", lam("a", app(var("f"), var("a"))))
}

macro_rules! church_num {
    ($($typ:ty),*) => {
        $(
            impl From<$typ> for Term {
                fn from(val: $typ) -> Self {
                    lam("f", lam("a", (0..val).fold(var("a"), |acc, _| app(var("f"), acc))))
                }
            }
        )*
    }
}

church_num!(u8, u16, u32, u64, u128, usize);

/// Successor : n + 1
///
/// SUCC ≡ λnfa.f(nfa)
pub fn succ() -> Term {
    lam(
        "n",
        lam(
            "f",
            lam("a", app(var("f"), app![var("n"), var("f"), var("a")])),
        ),
    )
}

#[cfg(test)]
mod tests;
