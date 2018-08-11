//! A lambda calculus parser and interpreter.

#![doc(html_root_url = "https://docs.rs/lamcal/0.1.0")]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
//    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unstable_features,
    unused_import_braces,
    unused_qualifications,
)]

#[cfg(test)]
#[macro_use]
extern crate galvanic_assert;
#[cfg(test)]
#[macro_use]
extern crate proptest;

#[macro_use]
extern crate combine;

#[macro_use]
mod term;
mod parser;
mod reduction;

pub use self::parser::{expression, ParseError, Parser, Stream};
pub use self::reduction::{
    alpha, apply, reduce, substitute, BetaReduce, CallByName, Enumerate, NormalOrder,
};
pub use self::term::{app, lam, var, Term, Var};
