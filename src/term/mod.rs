//! Syntax definitions for the lambda calculus.
//!
//! This implementation of the lambda calculus uses the classic notation.
//! Currently the De Bruin index notation is not supported.

use std::fmt::{self, Display};

pub fn var(name: impl Into<String>) -> Term {
    Term::var(name)
}

pub fn lam(param: impl Into<String>, body: Term) -> Term {
    Term::lam(Var(param.into()), body)
}

pub fn app(lhs: Term, rhs: Term) -> Term {
    Term::app(lhs, rhs)
}

/// Definition of a term in the lambda calculus.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    /// A variable (x)
    ///
    /// A character or string representing a parameter or mathematical/logical
    /// value.
    Var(String),
    /// An abstraction (λx.M)
    ///
    /// Function definition (M is a lambda term). The variable x becomes bound
    /// in the expression.
    Lam(Var, Box<Term>),
    /// An application (M N)
    ///
    /// Applying a function to an argument. M and N are lambda terms.
    App(Box<Term>, Box<Term>),
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Term::*;
        match self {
            Var(name) => write!(f, "{}", name),
            Lam(param, body) => write!(f, "λ{}.{}", param, body),
            App(lhs, rhs) => match **lhs {
                Lam(_, _) => write!(f, "({}) {}", lhs, rhs),
                _ => write!(f, "{} {}", lhs, rhs),
            },
        }
    }
}

impl Term {
    pub fn var(name: impl Into<String>) -> Self {
        Term::Var(name.into())
    }

    pub fn lam(param: Var, body: Term) -> Self {
        Term::Lam(param, Box::new(body))
    }

    pub fn app(lhs: Term, rhs: Term) -> Self {
        Term::App(Box::new(lhs), Box::new(rhs))
    }
}

impl<'a> From<&'a Term> for Term {
    fn from(expr: &Term) -> Self {
        expr.to_owned()
    }
}

impl From<Var> for Term {
    fn from(var: Var) -> Self {
        Term::Var(var.0)
    }
}

/// A variable with a given name.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub String);

impl Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Var {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Var {
    pub fn new(name: impl Into<String>) -> Self {
        Var(name.into())
    }

    pub fn unwrap(self) -> String {
        self.0
    }
}

#[macro_export]
macro_rules! app {
    ($term1:expr, $($term2:expr),+) => {
        {
            let mut term = $term1;
            $(term = app(term, $term2);)*
            term
        }
    }
}

#[cfg(test)]
mod tests;
