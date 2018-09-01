//! Definition of the lambda calculus term.
//!
//! This implementation of the lambda calculus uses the classic notation.
//! Currently the De Bruin index notation is not supported.

use std::fmt::{self, Display};
use std::ops::{Deref, DerefMut};

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
/// # use lamcal::{var, Term, VarName};
/// let variable = var("x");
///
/// assert_eq!(variable, Term::Var(VarName("x".to_string())));
/// ```
pub fn var(name: impl Into<String>) -> Term {
    Term::Var(VarName(name.into()))
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
/// # use lamcal::{lam, var, Term, VarName};
/// let abstraction = lam("x", var("x"));
///
/// assert_eq!(
///     abstraction,
///     Term::Lam(
///         VarName("x".to_string()),
///         Box::new(Term::Var(VarName("x".to_string())))
///     )
/// );
/// ```
pub fn lam(param: impl Into<String>, body: Term) -> Term {
    Term::Lam(VarName(param.into()), Box::new(body))
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
/// # use lamcal::{app, lam, var, Term, VarName};
/// let application = app(lam("x", var("x")), var("y"));
///
/// assert_eq!(
///     application,
///     Term::App(
///         Box::new(Term::Lam(
///             VarName("x".to_string()),
///             Box::new(Term::Var(VarName("x".to_string())))
///         )),
///         Box::new(Term::Var(VarName("y".to_string())))
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
    Var(VarName),
    /// An abstraction (λx.M)
    ///
    /// Function definition (M is a lambda term). The variable x becomes bound
    /// in the expression.
    Lam(VarName, Box<Term>),
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

impl From<VarName> for Term {
    fn from(var: VarName) -> Self {
        Term::Var(var)
    }
}

impl Term {
    /// Returns a variable's underlying variable name if this term is of variant
    /// `Term::Var`.
    pub fn unwrap_var(self) -> Option<VarName> {
        if let Term::Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns an abstraction's underlying bound variable name and body if
    /// this term is of variant `Term::Lam`.
    pub fn unwrap_lam(self) -> Option<(VarName, Term)> {
        if let Term::Lam(param, body) = self {
            Some((param, *body))
        } else {
            None
        }
    }

    /// Returns an application's underlying left side term and right side term
    /// if this term is of variant `Term::App`.
    pub fn unwrap_app(self) -> Option<(Term, Term)> {
        if let Term::App(left, right) = self {
            Some((*left, *right))
        } else {
            None
        }
    }

    /// Returns a reference to a variable's underlying name if this term is of
    /// variant `Term::Var`.
    pub fn unwrap_var_ref(&self) -> Option<&VarName> {
        if let Term::Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns references to an abstraction's underlying variable name and
    /// body if this term is of variant `Term::Lam`.
    pub fn unwrap_lam_ref(&self) -> Option<(&VarName, &Term)> {
        if let Term::Lam(param, body) = self {
            Some((param, body))
        } else {
            None
        }
    }

    /// Returns references to an application's underlying left side term and
    /// right side term if this term is of variant `Term::App`.
    pub fn unwrap_app_ref(&self) -> Option<(&Term, &Term)> {
        if let Term::App(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    /// Returns a mutable reference to a variable's underlying name if this
    /// term is of variant `Term::Var`.
    pub fn unwrap_var_mut(&mut self) -> Option<&mut VarName> {
        if let Term::Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns mutable references to an abstraction's underlying variable name
    /// and body if this term is of variant `Term::Lam`.
    pub fn unwrap_lam_mut(&mut self) -> Option<(&mut VarName, &mut Term)> {
        if let Term::Lam(param, body) = self {
            Some((param, body))
        } else {
            None
        }
    }

    /// Returns mutable references to an application's underlying left side term
    /// and right side term if this term is of variant `Term::App`.
    pub fn unwrap_app_mut(&mut self) -> Option<(&mut Term, &mut Term)> {
        if let Term::App(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }
}

/// A variable name.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarName(pub String);

impl Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for VarName {
    type Target = String;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.0
    }
}

impl DerefMut for VarName {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.0
    }
}

impl AsRef<str> for VarName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl VarName {
    /// Constructs a new variable of given name.
    pub fn new(name: impl Into<String>) -> Self {
        VarName(name.into())
    }

    /// Unwraps the `String` out of this variable name.
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
            let mut __term = $term1;
            $(__term = $crate::Term::App(Box::new(__term), Box::new($term2));)*
            __term
        }
    }
}
//
//#[macro_export]
//macro_rules! lam {
//    ($var1:expr $(, $var2:expr)*, $term:expr) => {
//        {
//            use std::mem;
//            let mut __term = Term::Lam(Var($var1.into()),
// Term::Var(String::new()));            $(if let Term::Lam(_, mut ref body) =
// __term {                let mut __term2 = Term::Lam(Var($var2.into(),
// Term::Var(String::new())));                mem::swap(&mut **body, &mut
// __term2);            })*
//            if let Term::Lam(_, mut ref body) = __term {
//                mem::swap(&mut **body, &mut $term);
//            }
//            __term
//        }
//    }
//}

#[cfg(test)]
mod tests;
