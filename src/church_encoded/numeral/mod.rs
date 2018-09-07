//! [Church numerals] and arithmetic operations
//!
//! Church encoding of natural numbers and arithmetic operations.
//!
//! * `Zero`
//! * `One`
//! * `Succ`
//!
//! [Church numerals]: https://en.wikipedia.org/wiki/Church_encoding#Church_numerals

#![allow(non_snake_case)]

use std::collections::HashSet;

use environment::Binding;
use term::{app, lam, var, Term};

/// Creates a set of bindings for all data types, data structures and operators
/// implemented in this module.
pub fn default_bindings() -> HashSet<Binding> {
    binds! {
        Zero => Zero(),
        One => One(),
        Succ => Succ(),
    }
}

/// Zero : 0 : apply f no times to a
///
/// Zero ≡ λfa.a
pub fn Zero() -> Term {
    lam("f", lam("a", var("a")))
}

/// Number : 1 : apply f once to a
///
/// One ≡ λfa.fa
pub fn One() -> Term {
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
/// Succ ≡ λnfa.f(nfa)
pub fn Succ() -> Term {
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
