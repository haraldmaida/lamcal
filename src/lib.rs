//! A lambda calculus parser and evaluator library.
//!
//! This crate implements a pure lambda calculus with the notation of a term
//! that is either a variable, a lambda abstraction or a function application.
//! A term is represented by the enum `Term` with its variants `Term::Var` for
//! variables, `Term::Lam` for lambda abstractions and `Term::App` for function
//! applications.
//!
//! The parser of the library supports the classic notation. The parser can be
//! invoked by calling the method `parse`. The input of the parse method can be
//! any data structure that provides an `Iterator` over `char` items.
//!
//! * variables can be single lower case letters or names with multiple
//!   characters where the first character must be a lower case letter. The
//!   characters following the first character can be lower case letters, digits
//!   the underscore `_` or the tick '\'' character.
//! * lambda abstractions start with the small greek letter lambda 'λ' or
//!   alternatively with a backslash '\\' for easier typing on traditional
//!   keyboards. Then follows a variable name as the parameter and a dot '.'
//!   that separates the parameter from the body.
//! * function applications are written as two terms side by side separated by
//!   whitespace.
//!
//! Function applications are left associative. Abstraction bodies are expanded
//! to the right as far as possible. Parenthesis can be used to group terms and
//! clarify precedence. Outermost parenthesis can be omitted.
//!
//! The parser fully supports unicode characters.
//!
//! The reduction system implements α-conversion and β-reduction. As their are
//! several possible ways (strategies) to implement the reduction rules for α-
//! and β-reduction those strategies are defined as traits. The trait
//! `AlphaRename` defines the strategy for renaming variables in case of
//! possible name clashes. The trait `BetaReduce` defines the strategy applied
//! when performing a β-reduction. The reduction system is designed based on
//! these traits so that users of the crate can easily implement their own
//! strategies and use them with all the functionality provided by this library.
//!
//! ## α-conversion
//!
//! ## β-reduction

#![doc(html_root_url = "https://docs.rs/lamcal/0.1.0")]
#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unstable_features,
    unused_import_braces,
    unused_qualifications,
)]

#[cfg(test)]
#[macro_use]
extern crate proptest;

#[macro_use]
extern crate failure;

#[macro_use]
mod term;
mod parser;
mod reduction;

pub use self::parser::{
    parse, parse_str, parse_tokens, pos, tokenize, tokenize_str, CharPosition, ParseError,
    ParseErrorKind, Token,
};
pub use self::reduction::{
    alpha, apply, reduce, substitute, AlphaRename, BetaReduce, CallByName, Enumerate, NormalOrder,
};
pub use self::term::{app, lam, var, Term, Var};
