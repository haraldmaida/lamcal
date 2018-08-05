//! Syntax definitions for the Lambda Calculus.

#[cfg(test)]
mod tests;

use std::fmt::{self, Display};

pub fn var(name: impl Into<String>) -> Term {
    Term::var(name)
}

pub fn lam(param: impl Into<String>, body: Term) -> Term {
    Term::lam(Var(param.into()), body)
}

pub fn app(expr1: Term, expr2: Term) -> Term {
    Term::app(expr1, expr2)
}

/// Definition of an expression in the Lambda Calculus.
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
        match self {
            Term::Var(name) => write!(f, "{}", name),
            Term::Lam(param, body) => write!(f, "λ{}.{}", param, body),
            Term::App(expr1, expr2) => write!(f, "({}) {}", expr1, expr2),
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

    pub fn app(expr1: Term, expr2: Term) -> Self {
        Term::App(Box::new(expr1), Box::new(expr2))
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
