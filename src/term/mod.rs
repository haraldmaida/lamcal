//! Definition of the lambda calculus term.
//!
//! This implementation of the lambda calculus uses the classic notation.
//! Currently the De Bruin index notation is not supported.

#[cfg(test)]
use proptest::strategy::Strategy;

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
/// This function combined with the functions [`lam`](fn.lam.html) and
/// [`app`](fn.app.html) let us construct any [`Term`](enum.Term.html) in the
/// untyped lambda calculus.
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
/// This function combined with the functions [`var`](fn.var.html) and
/// [`app`](fn.app.html) let us construct any [`Term`](enum.Term.html) in the
/// untyped lambda calculus.
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
/// This function combined with the functions [`var`](fn.var.html) and
/// [`lam`](fn.lam.html) let us construct any [`Term`](enum.Term.html) in the
/// untyped lambda calculus.
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

enum TermFormat<'a> {
    LParen,
    RParen,
    Space,
    ToFormat(&'a Term),
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::fmt::Write;
        use self::TermFormat::*;
        let mut to_format = Vec::with_capacity(7);
        to_format.push(ToFormat(self));
        while let Some(item) = to_format.pop() {
            match item {
                LParen => f.write_char('(')?,
                RParen => f.write_char(')')?,
                Space => f.write_char(' ')?,
                ToFormat(term) => match term {
                    Var(name) => f.write_str(&name)?,
                    Lam(param, body) => {
                        write!(f, "λ{}.", &param)?;
                        to_format.push(ToFormat(&**body));
                    },
                    App(lhs, rhs) => match (&**lhs, &**rhs) {
                        (App(_, _), App(_, _)) => {
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(LParen);
                            to_format.push(Space);
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**lhs));
                            to_format.push(LParen);
                        },
                        (Lam(_, _), App(_, _)) => {
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(LParen);
                            to_format.push(Space);
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**lhs));
                            to_format.push(LParen);
                        },
                        (Lam(_, _), Lam(_, _)) => {
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(LParen);
                            to_format.push(Space);
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**lhs));
                            to_format.push(LParen);
                        },
                        (_, App(_, _)) => {
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(LParen);
                            to_format.push(Space);
                            to_format.push(ToFormat(&**lhs));
                        },
                        (_, Lam(_, _)) => {
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(LParen);
                            to_format.push(Space);
                            to_format.push(ToFormat(&**lhs));
                        },
                        (Lam(_, _), _) => {
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(Space);
                            to_format.push(RParen);
                            to_format.push(ToFormat(&**lhs));
                            to_format.push(LParen);
                        },
                        (_, _) => {
                            to_format.push(ToFormat(&**rhs));
                            to_format.push(Space);
                            to_format.push(ToFormat(&**lhs));
                        },
                    },
                },
            }
        }
        Ok(())
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

impl Term {
    /// Returns a set of references to all free variables in this term.
    pub fn free_vars(&self) -> HashSet<&VarName> {
        let mut free_vars = HashSet::with_capacity(4);
        let mut to_check: Vec<(&Term, HashSet<&VarName>)> = Vec::with_capacity(2);
        to_check.push((self, HashSet::with_capacity(4)));
        while let Some((term, mut bound_vars)) = to_check.pop() {
            match *term {
                Var(ref name) => {
                    if !bound_vars.contains(name) {
                        free_vars.insert(name);
                    }
                },
                Lam(ref param, ref body) => {
                    bound_vars.insert(param);
                    to_check.push((&*body, bound_vars));
                },
                App(ref lhs, ref rhs) => {
                    to_check.push((&*rhs, bound_vars.clone()));
                    to_check.push((&*lhs, bound_vars));
                },
            }
        }
        free_vars
    }
}

#[cfg(test)]
#[allow(dead_code)]
pub fn any_application() -> impl Strategy<Value = Term> {
    (any_term(), any_term()).prop_map(|(lhs, rhs)| App(lhs.into(), rhs.into()))
}

#[cfg(test)]
#[allow(dead_code)]
pub fn any_abstraction() -> impl Strategy<Value = Term> {
    (any_short_var_name(), any_term()).prop_map(|(param, body)| Lam(param, body.into()))
}

#[cfg(test)]
pub fn any_term() -> impl Strategy<Value = Term> {
    any_short_variable().prop_recursive(23, 500, 3, |inner| {
        prop_oneof!{
            inner.clone(),
            (any_short_var_name(), inner.clone()).prop_map(|(param, body)| Lam(param, body.into())),
            (inner.clone(), inner.clone()).prop_map(|(lhs, rhs)| App(lhs.into(), rhs.into())),
        }
    })
}

#[cfg(test)]
#[allow(dead_code)]
pub fn any_variable() -> impl Strategy<Value = Term> {
    any_var_name().prop_map(Var)
}

#[cfg(test)]
pub fn any_short_variable() -> impl Strategy<Value = Term> {
    any_short_var_name().prop_map(Var)
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

#[cfg(test)]
pub fn any_var_name() -> impl Strategy<Value = VarName> {
    any_identifier_().prop_map(VarName)
}

#[cfg(test)]
pub fn any_short_var_name() -> impl Strategy<Value = VarName> {
    any_short_identifier_().prop_map(VarName)
}

#[cfg(test)]
pub fn any_identifier() -> impl Strategy<Value = String> {
    any_identifier_()
}

#[cfg(test)]
prop_compose! {
    fn any_identifier_()(
        name in "[A-Za-z0-9][A-Za-z0-9_']*",
    ) -> String {
        name
    }
}

#[cfg(test)]
prop_compose! {
    fn any_short_identifier_()(
        name in "[A-Za-z0-9][A-Za-z0-9_']?",
    ) -> String {
        name
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
