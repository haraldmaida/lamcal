//! Definition of the lambda calculus term.
//!
//! This implementation of the lambda calculus uses the classic notation.
//! Currently the De Bruin index notation is not supported.

use std::collections::HashSet;
use std::fmt::{self, Display};
use std::ops::{Deref, DerefMut};

use self::Term::*;

/// Constructs a variable of the given name.
///
/// This is a convenience function for constructing a [`Term`](enum.Term.html)
/// of variant `Term::Var` in a readable form with minimal keystrokes. It takes
/// any value that can be converted into a `String` and returns
/// `Term::Var(VarName(name))`.
///
/// This function combined with the functions [`lam`](fn.lam.html),
/// [`app`](fn.app.html) and [`con`](fn.con.html) let us construct any
/// [`Term`](enum.Term.html) in the untyped lambda calculus.
///
/// # Examples
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{var, Term, VarName};
/// let variable = var("x");
///
/// assert_eq!(variable, Term::Var(VarName("x".to_string())));
/// ```
pub fn var(name: impl Into<String>) -> Term {
    Var(VarName(name.into()))
}

/// Constructs a lambda abstraction with given parameter and body.
///
/// This is a convenience function for constructing a [`Term`](enum.Term.html)
/// of variant `Term::Lam` in a readable form with minimal keystrokes. It takes
/// any value that can be converted into a `String` to form a bound variable
/// (the parameter of the abstraction) and a `Term` for the body of the
/// abstraction.
///
/// This function combined with the functions [`var`](fn.var.html),
/// [`app`](fn.app.html) and [`con`](fn.con.html) let us construct any
/// [`Term`](enum.Term.html) in the untyped lambda calculus.
///
/// # Examples
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
    Lam(VarName(param.into()), Box::new(body))
}

/// Constructs a function application with the `lhs` term to be applied to the
/// `rhs` term.
///
/// This is a convenience function for constructing a [`Term`](enum.Term.html)
/// of variant `Term::App` in a readable form with minimal keystrokes. It takes
/// two `Term`s as its input and returns a `Term::App` with the first `Term` to
/// be applied to the second `Term`.
///
/// This function combined with the functions [`var`](fn.var.html),
/// [`lam`](fn.lam.html) and [`con`](fn.con.html) let us construct any
/// [`Term`](enum.Term.html) in the untyped lambda calculus.
///
/// # Examples
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
    App(Box::new(lhs), Box::new(rhs))
}

/// Constructs a named constant of the given name.
///
/// This is a convenience function for constructing a [`Term`](enum.Term.html)
/// of variant `Term::Const` in a readable form with minimal keystrokes. It
/// takes any value that can be converted into a `String` and returns
/// `Term::Const(ConstName(name))`.
///
/// This function combined with the functions [`var`](fn.var.html),
/// [`lam`](fn.lam.html) and [`app`](fn.app.html) let us construct any
/// [`Term`](enum.Term.html) in the untyped lambda calculus.
///
/// # Examples
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{app, con, ConstName, Term};
/// let application = app(con("S"), con("K"));
///
/// assert_eq!(
///     application,
///     Term::App(
///         Box::new(Term::Const(ConstName("S".to_string()))),
///         Box::new(Term::Const(ConstName("K".to_string()))),
///     )
/// );
/// ```
pub fn con(name: impl Into<String>) -> Term {
    Const(ConstName(name.into()))
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
    /// A named constant (K)
    ///
    /// A predefined lambda term that is referenced by the name K.
    Const(ConstName),
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
            Const(name) => write!(f, "{}", name),
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
        Var(var)
    }
}

impl From<ConstName> for Term {
    fn from(con: ConstName) -> Self {
        Const(con)
    }
}

impl Term {
    /// Returns a variable's underlying variable name if this term is of variant
    /// `Term::Var`.
    pub fn unwrap_var(self) -> Option<VarName> {
        if let Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns an abstraction's underlying bound variable name and body if
    /// this term is of variant `Term::Lam`.
    pub fn unwrap_lam(self) -> Option<(VarName, Term)> {
        if let Lam(param, body) = self {
            Some((param, *body))
        } else {
            None
        }
    }

    /// Returns an application's underlying left side term and right side term
    /// if this term is of variant `Term::App`.
    pub fn unwrap_app(self) -> Option<(Term, Term)> {
        if let App(left, right) = self {
            Some((*left, *right))
        } else {
            None
        }
    }

    /// Returns a named constant's underlying name if this term is of variant
    /// `Term::Const`.
    pub fn unwrap_const(self) -> Option<ConstName> {
        if let Const(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns a reference to a variable's underlying name if this term is of
    /// variant `Term::Var`.
    pub fn unwrap_var_ref(&self) -> Option<&VarName> {
        if let Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns references to an abstraction's underlying variable name and
    /// body if this term is of variant `Term::Lam`.
    pub fn unwrap_lam_ref(&self) -> Option<(&VarName, &Term)> {
        if let Lam(param, body) = self {
            Some((param, body))
        } else {
            None
        }
    }

    /// Returns references to an application's underlying left side term and
    /// right side term if this term is of variant `Term::App`.
    pub fn unwrap_app_ref(&self) -> Option<(&Term, &Term)> {
        if let App(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    /// Returns a reference to a named constant's underlying name if this term
    /// is of variant `Term::Const`.
    pub fn unwrap_const_ref(&self) -> Option<&ConstName> {
        if let Const(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns a mutable reference to a variable's underlying name if this
    /// term is of variant `Term::Var`.
    pub fn unwrap_var_mut(&mut self) -> Option<&mut VarName> {
        if let Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns a mutable references to an abstraction's underlying variable
    /// name and body if this term is of variant `Term::Lam`.
    pub fn unwrap_lam_mut(&mut self) -> Option<(&mut VarName, &mut Term)> {
        if let Lam(param, body) = self {
            Some((param, body))
        } else {
            None
        }
    }

    /// Returns a mutable references to an application's underlying left side
    /// term and right side term if this term is of variant `Term::App`.
    pub fn unwrap_app_mut(&mut self) -> Option<(&mut Term, &mut Term)> {
        if let App(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    /// Returns a mutable reference to a named constant's underlying name if
    /// this term is of variant `Term::Const`.
    pub fn unwrap_const_mut(&mut self) -> Option<&mut ConstName> {
        if let Const(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns a set of references to all free variables in this term.
    pub fn free_vars(&self) -> HashSet<&VarName> {
        let mut free_vars = HashSet::new();
        let mut terms = vec![(self, HashSet::new())];
        while !terms.is_empty() {
            let (term, mut bound_vars) = terms.remove(0);
            match *term {
                Var(ref name) => {
                    if !bound_vars.contains(name) {
                        free_vars.insert(name);
                    }
                },
                Lam(ref param, ref body) => {
                    bound_vars.insert(param);
                    terms.push((&*body, bound_vars));
                },
                App(ref lhs, ref rhs) => {
                    terms.push((&*lhs, bound_vars.clone()));
                    terms.push((&*rhs, bound_vars));
                },
                Const(_) => {},
            }
        }
        free_vars
    }
}

/// A name of a variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    /// Constructs a new variable name.
    pub fn new(name: impl Into<String>) -> Self {
        VarName(name.into())
    }

    /// Unwraps the `String` out of this variable name.
    pub fn unwrap(self) -> String {
        self.0
    }
}

/// A name of a constant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConstName(pub String);

impl Display for ConstName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for ConstName {
    type Target = String;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.0
    }
}

impl DerefMut for ConstName {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.0
    }
}

impl AsRef<str> for ConstName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl ConstName {
    /// Constructs a new constant name.
    pub fn new(name: impl Into<String>) -> Self {
        ConstName(name.into())
    }

    /// Unwraps the `String` out of this constant name.
    pub fn unwrap(self) -> String {
        self.0
    }
}

/// Constructs a `Term` from a sequence of function applications.
///
/// The app! macro can be used to conveniently construct a `Term` from a
/// sequence of function applications without all the nested calls of the `app`
/// functions which would be the alternative way.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate lamcal;
/// use lamcal::{app, lam, var};
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
