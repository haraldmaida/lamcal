//! A lambda calculus parser and evaluator library.
//!
//! This crate implements an untyped [lambda calculus] with the notation of a
//! term as the main data type. A term is either a variable, a lambda
//! abstraction or a function application.
//!
//! ## the term
//!
//! The lambda term is the main data type in *lamcal*. A lambda term is
//! represented by the enum [`Term`](enum.Term.html). Its variants are:
//!
//! * `Term::Var` for variables
//! * `Term::Lam` for lambda abstractions
//! * `Term::App` for function applications
//!
//! We can construct lambda terms programmatically by using the convenient
//! functions [`var`](fn.var.html), [`lam`](fn.lam.html), [`app`](fn.app.html)
//! and the macro [`app!`](macro.app.html).
//!
//! ## the parser
//!
//! The parser of the library supports the classic notation. The parser is
//! invoked by calling the function [`parse`](fn.parse.html). The input of the
//! parse method can be any data structure that provides an `Iterator` over
//! `char` items. To parse a term from a `str` slice we can use the function
//! [`parse_str`](fn.parse_str.html).
//!
//! * Variables can be single unicode alphanumeric characters or names with
//!   multiple characters where the first character must be a unicode
//!   alphanumeric character. The characters following the first character can
//!   be unicode letters, digits, the underscore `_` or the tick `'` character.
//! * Lambda abstractions start with the greek lowercase letter lambda `λ` or
//!   alternatively with a backslash `\` for easier typing on traditional
//!   keyboards. Then follows a variable name as the parameter and a dot `.`
//!   that separates the parameter from the body.
//! * Function applications are written as two terms side by side separated by
//!   whitespace.
//! * Parenthesis can be used to group terms and clarify precedence. Outermost
//!   parenthesis can be omitted.
//! * Function applications are left associative. This means the expression
//!   `(λx.x) y z` is equivalent to the expression <br/>`((λx.x) y) z`.
//! * Abstraction bodies are expanded to the right as far as possible. This
//!   means the expression `λx.x y z` is equivalent to the expression
//!   `λx.(x y z)`. To apply this abstraction to a variable `a` we have to use
//!   parenthesis like so `(λx.x y z) a`.
//!
//! ## the reduction system
//!
//! The reduction system implements α-conversion and β-reduction.
//!
//! The functions of the reduction system are provided in two variants: as
//! standalone function and associated function on `Term`. The standalone
//! function takes the input term by reference and returns the result in a new
//! instance of `Term` while the input term remains unchanged. The functions
//! associated on `Term` take the term by mutable reference and apply the
//! reduction on the term in place.
//!
//! As their are several possible ways (strategies) to implement the reduction
//! rules for α- and β-reduction those strategies are defined as traits. The
//! reduction system is designed based on these traits so that users of the
//! crate can easily implement their own strategies and use them with all the
//! functionality provided by this library.
//!
//! ### α-conversion
//!
//! α-conversion renames bound variables if the name conflicts with a free
//! variable in a function application.
//!
//! To execute α-conversion on a term we use either the standalone function
//! [`alpha`](fn.alpha.html) or the associated function
//! [`Term::alpha`](enum.Term.html#method.alpha). We must tell those functions
//! which strategy to use for renaming variables. The strategy is specified as
//! a type parameter, <br/>e.g. `alpha::<Enumerate>(&expr)`.
//!
//! The trait [`AlphaRename`](trait.AlphaRename.html) defines the strategy for
//! renaming variables in case of possible name clashes. The provided
//! implementations are [`Enumerate`](struct.Enumerate.html) and
//! [`Prime`](struct.Prime.html).
//!
//! ### β-reduction
//!
//! β-reduction evaluates function applications according a chosen strategy.
//!
//! To execute β-reduction on a term we use either the standalone function
//! [`reduce`](fn.reduce.html) or the associated function
//! [`Term::reduce`](enum.Term.html#method.reduce). We must tell those functions
//! which strategy we want ot use for reduction. The strategy is specified as
//! a type parameter, <br/>e.g. `reduce::<NormalOrder>(&expr)`.
//!
//! The trait [`BetaReduce`](trait.BetaReduce.html) defines the strategy
//! applied when performing a β-reduction. The provided implementations are:
//!
//! * [`CallByName`](struct.CallByName.html):
//!   call-by-name reduction to weak head normal form
//! * [`NormalOrder`](struct.NormalOrder.html):
//!   normal-order reduction to normal form
//! * [`CallByValue`](struct.CallByValue.html):
//!   call-by-value reduction to weak normal form
//! * [`ApplicativeOrder`](struct.ApplicativeOrder.html):
//!   applicative-order reduction to normal form
//! * [`HybridApplicativeOrder`](struct.HybridApplicativeOrder.html):
//!   hybrid-applicative-order reduction to normal form
//! * [`HeadSpine`](struct.HeadSpine.html):
//!   head-spine reduction to head normal form
//! * [`HybridNormalOrder`](struct.HybridNormalOrder.html):
//!   hybrid-normal-order reduction to normal form
//!
//! [lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus

#![doc(html_root_url = "https://docs.rs/lamcal/0.2.0")]
#![warn(
    bare_trait_objects,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unstable_features,
    unused_extern_crates,
    unused_import_braces,
    unused_qualifications,
)]

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
#[cfg(test)]
#[macro_use]
extern crate proptest;

#[cfg(feature = "failure")]
#[macro_use]
extern crate failure;

#[macro_use]
mod term;
#[macro_use]
mod environment;
mod parser;
mod reduction;

pub mod church_encoded;
pub mod combinator;

pub use self::environment::{bind, Binding, Environment};
pub use self::parser::{
    hint, parse, parse_str, parse_tokens, pos, tokenize, tokenize_str, CharPosition, Hint,
    ParseError, ParseErrorKind, Token,
};
pub use self::reduction::{
    alpha, apply, evaluate, expand, reduce, substitute, AlphaRename, ApplicativeOrder, BetaReduce,
    CallByName, CallByValue, Enumerate, HeadSpine, HybridApplicativeOrder, HybridNormalOrder,
    NormalOrder, Prime,
};
pub use self::term::{app, lam, var, Term, VarName};

impl Default for Environment {
    /// Creates an `Environment` containing predefined bindings to all the
    /// standard terms, combinators and data encoding terms that are provided
    /// by this crate.
    fn default() -> Self {
        let mut env = Environment::new();
        env.extend(combinator::default_bindings());
        env.extend(church_encoded::default_bindings());
        env
    }
}
