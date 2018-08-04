//! Syntax definitions for the Lambda Calculus.

#[cfg(test)]
mod tests;

use std::fmt::{self, Display};

pub fn var(name: impl Into<String>) -> Expr {
    Expr::var(name)
}

pub fn lam(param: impl Into<String>, body: Expr) -> Expr {
    Expr::lam(Var(param.into()), body)
}

pub fn app(expr1: Expr, expr2: Expr) -> Expr {
    Expr::app(expr1, expr2)
}

/// Definition of an expression in the Lambda Calculus.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    /// A variable (x)
    ///
    /// A character or string representing a parameter or mathematical/logical
    /// value.
    Var(String),
    /// An abstraction (λx.M)
    ///
    /// Function definition (M is a lambda term). The variable x becomes bound
    /// in the expression.
    Lam(Var, Box<Expr>),
    /// An application (M N)
    ///
    /// Applying a function to an argument. M and N are lambda terms.
    App(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lam(param, body) => write!(f, "λ{}.{}", param, body),
            Expr::App(expr1, expr2) => write!(f, "({}) {}", expr1, expr2),
        }
    }
}

impl Expr {
    pub fn var(name: impl Into<String>) -> Self {
        Expr::Var(name.into())
    }

    pub fn lam(param: Var, body: Expr) -> Self {
        Expr::Lam(param, Box::new(body))
    }

    pub fn app(expr1: Expr, expr2: Expr) -> Self {
        Expr::App(Box::new(expr1), Box::new(expr2))
    }
}

impl<'a> From<&'a Expr> for Expr {
    fn from(expr: &Expr) -> Self {
        expr.to_owned()
    }
}

impl From<Var> for Expr {
    fn from(var: Var) -> Self {
        Expr::Var(var.0)
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
