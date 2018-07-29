//! Syntax definitions for the Lambda Calculus.

#[cfg(test)]
mod tests;

use std::fmt::{self, Display};

pub fn var(name: impl Into<String>) -> Expr {
    Expr::var(name)
}

pub fn lam(param: impl Into<String>, body: Expr) -> Expr {
    Expr::lambda(Var(param.into()), body)
}

pub fn apply(expr1: Expr, expr2: Expr) -> Expr {
    Expr::apply(expr1, expr2)
}

/// Definition of an expression in the Lambda Calculus.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// a variable
    Var(String),
    /// a lambda abstraction
    Lambda { param: Var, body: Box<Expr> },
    /// a function application
    Apply { expr1: Box<Expr>, expr2: Box<Expr> },
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lambda { param, body } => write!(f, "λ{}.{}", param, body),
            Expr::Apply { expr1, expr2 } => write!(f, "({}) {}", expr1, expr2),
        }
    }
}

impl Expr {
    pub fn var(name: impl Into<String>) -> Self {
        Expr::Var(name.into())
    }

    pub fn lambda(param: Var, body: Expr) -> Self {
        Expr::Lambda {
            param,
            body: Box::new(body),
        }
    }

    pub fn apply(expr1: Expr, expr2: Expr) -> Self {
        Expr::Apply {
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
        }
    }
}

impl From<Var> for Expr {
    fn from(var: Var) -> Self {
        Expr::Var(var.0)
    }
}

/// A variable with a given name.
#[derive(Debug, Clone, PartialEq)]
pub struct Var(pub String);

impl Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Var {
    pub fn new(name: impl Into<String>) -> Self {
        Var(name.into())
    }

    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn unwrap(self) -> String {
        self.0
    }
}
