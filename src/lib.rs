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

pub mod parser;
pub mod reduction;
pub mod syntax;

pub use parser::{expression, ParseError, Parser, Stream};
pub use syntax::{app, lam, var, Term, Var};
