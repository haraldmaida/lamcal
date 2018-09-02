//! [Standard term]s and [combinator]s
//!
//! This module defines [standard term]s and [combinator]s with commonly
//! accepted names. A combinator is a lambda calculus term that contains no
//! free variables.
//!
//! The combinators are collected from these sources:
//!
//! * [SKI] system by Moses Schönfinkel and Haskell Curry
//! * [BCKW] system by Haskell Curry
//! * [fixed-point combinator]s by Haskell Curry
//! * the self-application combinator ω
//! * the divergent combinator Ω
//! * the reverse application (thrush) combinator R
//!
//! The standard terms and combinators defined in this module are also builtin
//! named constants in the default environment created by
//! `Environment::default()`.
//!
//! [standard term]: https://en.wikipedia.org/w/index.php?title=Lambda_calculus#Standard_terms
//! [combinator]: https://en.wikipedia.org/wiki/Combinatory_logic#Combinatory_calculi
//! [fixed-point combinator]: https://en.wikipedia.org/wiki/Fixed-point_combinator
//! [SKI]: https://en.wikipedia.org/wiki/SKI_combinator_calculus
//! [BCKW]: https://en.wikipedia.org/wiki/B,_C,_K,_W_system
//! [Iota]: https://en.wikipedia.org/wiki/Iota_and_Jot

#![allow(non_snake_case)]

use std::collections::HashSet;

use environment::Binding;
use term::{app, lam, var, Term};

/// Creates a set of bindings for all combinators of the [SKI] system.
///
/// The returned `HashSet` contains a binding for the combinators:
///
/// * S - Starling - Substitution
/// * K - Kestrel  - Constant
/// * I - Idiot    - Identity
///
/// [SKI]: https://en.wikipedia.org/wiki/SKI_combinator_calculus
pub fn SKI() -> HashSet<Binding> {
    binds! {
        S => S(),
        K => K(),
        I => I(),
    }
}

/// I - Idiot - Identity combinator
///
/// I ≡ λa.a
pub fn I() -> Term {
    lam("a", var("a"))
}

/// K - Kestrel - Constant combinator (TRUE)
///
/// K ≡ λab.a
pub fn K() -> Term {
    lam("a", lam("b", var("a")))
}

/// S - Starling - Substitution
///
/// S ≡ λabc.ac(bc)
pub fn S() -> Term {
    lam(
        "a",
        lam(
            "b",
            lam("c", app![var("a"), var("c"), app(var("b"), var("c"))]),
        ),
    )
}
