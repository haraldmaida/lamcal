//! Syntax definitions for the lambda calculus.

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

/// Definition of an term in the lambda calculus.
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

    /// Returns whether this `Term` is a [beta redex].
    ///
    /// A beta redex is a term of the form (λx.A) M
    ///
    /// The term redex, short for reducible expression, refers to subterms that
    /// can be reduced by one of the reduction rules.
    ///
    /// [beta redex]: https://en.wikipedia.org/wiki/Beta_normal_form#Beta_reduction
    pub fn is_beta_redex(&self) -> bool {
        use self::Term::*;
        match *self {
            App(ref expr1, _) => match **expr1 {
                Lam(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }

    /// Returns whether this `Term` is a [beta normal form].
    ///
    /// A beta normal form is a term that does not contain any beta redex,
    /// i.e. that cannot be further reduced.
    ///
    /// [beta normal form]: https://en.wikipedia.org/wiki/Beta_normal_form
    pub fn is_beta_normal(&self) -> bool {
        if self.is_beta_redex() {
            false
        } else {
            use self::Term::*;
            match *self {
                App(ref expr1, ref expr2) => expr1.is_beta_normal() && expr2.is_beta_normal(),
                _ => true,
            }
        }
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
