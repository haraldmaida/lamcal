//! [Standard term]s and [combinator]s
//!
//! This module defines [standard term]s and [combinator]s with commonly
//! accepted names. A combinator is a closed lambda expression, meaning that it
//! has no free variables.
//!
//! The combinators are collected from these sources:
//!
//! * [SKI] system by Moses Schönfinkel and Haskell Curry
//! * [BCKW] system by Haskell Curry
//! * [fixed-point combinator]s by Haskell Curry
//! * the self-application combinator ω
//! * the divergent combinator Ω
//! * the reverse application (thrush) combinator
//!
//! The standard terms and combinators defined in this module are also
//! predefined named constants in the default environment which can be created
//! by calling `Environment::default()`.
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

/// Creates a set of bindings for all combinators implemented in this module.
pub fn default_bindings() -> HashSet<Binding> {
    binds! {
        I => I(),
        K => K(),
        S => S(),
        B => B(),
        C => C(),
        W => W(),
        ω => M(),
        M => M(),
        Ω => MM(),
        MM => MM(),
        T => T(),
        U => U(),
        V => V(),
        Y => Y(),
        Z => Z(),
        Θ => UU(),
        UU => UU(),
    }
}

/// Creates a set of bindings for all combinators of the [SKI] system.
///
/// The returned `HashSet` contains a binding for the combinators:
///
/// * S - Starling - Substitution
/// * K - Kestrel  - Constant
/// * I - Idiot    - Identity
///
/// [SKI]: https://en.wikipedia.org/wiki/SKI_combinator_calculus
pub fn ski_set() -> HashSet<Binding> {
    binds! {
        S => S(),
        K => K(),
        I => I(),
    }
}

/// I - Idiot - Identity combinator
///
/// I ≡ λa.a ≡ S K K
pub fn I() -> Term {
    lam("a", var("a"))
}

/// K - Kestrel - Constant combinator (TRUE)
///
/// K ≡ λab.a
pub fn K() -> Term {
    lam("a", lam("b", var("a")))
}

/// S - Starling - Substitution combinator
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

/// Creates a set of bindings for all combinators of the [BCKW] system.
///
/// The returned `HashSet` contains a binding for the combinators:
///
/// * B - Bluebird - Composition
/// * C - Cardinal - Swapping
/// * K - Kestrel  - Constant
/// * W - Warbler  - Duplication
///
/// [BCKW]: https://en.wikipedia.org/wiki/B,_C,_K,_W_system
pub fn bckw_set() -> HashSet<Binding> {
    binds! {
        B => B(),
        C => C(),
        K => K(),
        W => W(),
    }
}

/// B - Bluebird - Composition combinator
///
/// B ≡ λabc.a(bc) ≡ S (K S) K
pub fn B() -> Term {
    lam(
        "a",
        lam("b", lam("c", app(var("a"), app(var("b"), var("c"))))),
    )
}

/// C - Cardinal - Swapping combinator
///
/// C ≡ λabc.acb ≡ S (B B S) (K K)
pub fn C() -> Term {
    lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")])))
}

/// W - Warbler - Duplication combinator
///
/// W ≡ λab.abb ≡ C (B M R)
pub fn W() -> Term {
    lam("a", lam("b", app![var("a"), var("b"), var("b")]))
}

/// ω - M - Mockingbird - Self-application combinator
///
/// ω ≡ M ≡ λa.aa ≡ S I I
pub fn M() -> Term {
    lam("a", app![var("a"), var("a")])
}

/// Ω - Omega - Divergent combinator
///
/// Ω ≡ ω ω ≡ M M
pub fn MM() -> Term {
    app(
        lam("a", app![var("a"), var("a")]),
        lam("a", app![var("a"), var("a")]),
    )
}

/// T - Thrush - Reverse application combinator
///
/// T ≡ λab.ba ≡ C I
pub fn T() -> Term {
    lam("a", lam("b", app![var("b"), var("a")]))
}

/// U - Turing
///
/// U ≡ λab.b(aab) ≡ L O
pub fn U() -> Term {
    lam(
        "a",
        lam("b", app(var("b"), app![var("a"), var("a"), var("b")])),
    )
}

/// V - Vireo - Pairing combinator (PAIR)
///
/// V ≡ λabc.cab ≡ B C T
pub fn V() -> Term {
    lam("a", lam("b", lam("c", app![var("c"), var("a"), var("b")])))
}

/// Y - lazy fixed-point combinator
///
/// Y ≡ λf.(λa.f(aa))(λa.f(aa))
///
/// discovered by Haskell Curry.
pub fn Y() -> Term {
    lam(
        "f",
        app(
            lam("a", app(var("f"), app(var("a"), var("a")))),
            lam("a", app(var("f"), app(var("a"), var("a")))),
        ),
    )
}

/// Z - strict fixed-point combinator
///
/// Z ≡ λf.(λa.f(λb.aab))(λa.f(λb.aab))
pub fn Z() -> Term {
    lam(
        "f",
        app(
            lam(
                "a",
                app(var("f"), lam("b", app![var("a"), var("a"), var("b")])),
            ),
            lam(
                "a",
                app(var("f"), lam("b", app![var("a"), var("a"), var("b")])),
            ),
        ),
    )
}

/// Θ - Turing fixed-point combinator
///
/// Θ ≡ (λab.b(aab))(λab.b(aab)) ≡ U U
///
/// discovered by Alan Turing.
pub fn UU() -> Term {
    app(
        lam(
            "a",
            lam("b", app(var("b"), app![var("a"), var("a"), var("b")])),
        ),
        lam(
            "a",
            lam("b", app(var("b"), app![var("a"), var("a"), var("b")])),
        ),
    )
}
