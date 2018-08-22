//! Definition of the lambda calculus term.
//!
//! This implementation of the lambda calculus uses the classic notation.
//! Currently the De Bruin index notation is not supported.

use std::fmt::{self, Display};

/// Constructs a variable of the given name.
///
/// This is a convenience function for constructing a
/// [`Term`](enum.Term.html) of variant `Term::Var` in a readable form with
/// minimal keystrokes. It takes any value that can be converted into a `String`
/// and returns `Term::Var(name)`.
///
/// This function combined with the functions [`lam`](fn.lam.html) and
/// [`app`](fn.app.html) let us construct any [`Term`](enum.Term.html) in the
/// pure lambda calculus.
///
/// # Example
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{var, Term};
/// let variable = var("x");
///
/// assert_eq!(variable, Term::Var("x".to_string()));
/// ```
pub fn var(name: impl Into<String>) -> Term {
    Term::Var(name.into())
}

/// Constructs a lambda abstraction with given parameter and body.
///
/// This is a convenience function for constructing a [`Term`](enum.Term.html)
/// of variant `Term::Lam` in a readable form with minimal keystrokes. It takes
/// any value that can be converted into a `String` to form a bound variable
/// (the parameter of the abstraction) and a `Term` for the body of the
/// abstraction.
///
/// This function combined with the functions [`var`](fn.var.html) and
/// [`app`](fn.app.html) let us construct any [`Term`](enum.Term.html) in the
/// pure lambda calculus.
///
/// # Example
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{lam, var, Term, Var};
/// let abstraction = lam("x", var("x"));
///
/// assert_eq!(
///     abstraction,
///     Term::Lam(Var("x".to_string()), Box::new(Term::Var("x".to_string())))
/// );
/// ```
pub fn lam(param: impl Into<String>, body: Term) -> Term {
    Term::Lam(Var(param.into()), Box::new(body))
}

/// Constructs a function application with the `lhs` term to be applied to the
/// `rhs` term.
///
/// This is a convenience function for constructing a [`Term`](enum.Term.html)
/// of variant `Term::App` in a readable form with minimal keystrokes. It takes
/// two `Term`s as its input and returns a `Term::App` with the first `Term` to
/// be applied to the second `Term`.
///
/// This function combined with the functions [`var`](fn.var.html) and
/// [`lam`](fn.lam.html) let us construct any [`Term`](enum.Term.html) in the
/// pure lambda calculus.
///
/// # Example
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{app, lam, var, Term, Var};
/// let application = app(lam("x", var("x")), var("y"));
///
/// assert_eq!(
///     application,
///     Term::App(
///         Box::new(Term::Lam(
///             Var("x".to_string()),
///             Box::new(Term::Var("x".to_string()))
///         )),
///         Box::new(Term::Var("y".to_string()))
///     )
/// );
/// ```
pub fn app(lhs: Term, rhs: Term) -> Term {
    Term::App(Box::new(lhs), Box::new(rhs))
}

/// A term in the lambda calculus.
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
    /// Constructs a new variable of given name.
    pub fn new(name: impl Into<String>) -> Self {
        Var(name.into())
    }

    /// Unwraps the name out of the variable.
    pub fn unwrap(self) -> String {
        self.0
    }
}

/// The app! macro can be used to conveniently construct an sequence of function
/// applications.
///
/// # Example
///
/// ```
/// #[macro_use]
/// extern crate lamcal;
/// # use lamcal::{app, var, lam};
///
/// # fn main() {
/// let expr = app![
///     lam("x", var("x")),
///     lam("y", app!(var("x"), var("y"))),
///     var("z")
/// ];
///
/// assert_eq!(
///     expr,
///     app(
///         app(lam("x", var("x")), lam("y", app(var("x"), var("y")))),
///         var("z")
///     )
/// );
/// # }
/// ```
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
