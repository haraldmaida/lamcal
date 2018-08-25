//! Reduction system of the lambda calculus.
//!
//! [evaluation strategies]: https://en.wikipedia.org/wiki/Evaluation_strategy
//! [reduction strategies]: https://en.wikipedia.org/wiki/Lambda_calculus#Reduction_strategies

#[cfg(test)]
mod tests;

use std::collections::HashSet;
use std::marker::PhantomData;
use std::mem;

use term::{app, Term, Term::*, Var as VarT};

impl Term {
    /// Returns whether this `Term` is a [beta redex].
    ///
    /// A beta redex is a term of the form (λx.A) M
    ///
    /// The term redex, short for reducible expression, refers to subterms that
    /// can be reduced by one of the reduction rules.
    ///
    /// [beta redex]: https://en.wikipedia.org/wiki/Beta_normal_form#Beta_reduction
    pub fn is_beta_redex(&self) -> bool {
        match *self {
            App(ref lhs, _) => match **lhs {
                Lam(_, _) => true,
                _ => false,
            },
            Lam(_, ref body) => body.is_beta_redex(),
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
            match *self {
                App(ref lhs, ref rhs) => lhs.is_beta_normal() && rhs.is_beta_normal(),
                Lam(_, ref body) => body.is_beta_normal(),
                _ => true,
            }
        }
    }

    /// Performs an [α-conversion] on this `Term`.
    ///
    /// [α-conversion]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
    pub fn alpha<A>(&mut self)
    where
        A: AlphaRename,
    {
        alpha_rec::<A>(self, Context::new())
    }

    /// [Substitutes] all occurrences of `var` as free variables with the
    /// expression `rhs` recursively on the structure of this `Term`.
    ///
    /// [substitutes]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
    pub fn substitute(&mut self, var: &VarT, rhs: &Term) {
        substitute_rec(self, var, rhs)
    }

    /// Applies the expression `rhs` to the param of this lambda abstraction if
    /// this `Term` is of variant `Term::Lam` by recursively substituting all
    /// occurrences of the bound variable in the body of the lambda abstraction
    /// with the expression `rhs`.
    ///
    /// If this `Term` is not a lambda abstraction this function does nothing.
    ///
    /// To avoid name clashes this function performs α-conversions when
    /// appropriate. Therefore a strategy for α-conversion must be given as a
    /// type parameter.
    ///
    /// # Example
    ///
    /// ```
    /// # extern crate lamcal;
    /// # use lamcal::{app, lam, var, Enumerate};
    /// let mut expr = lam("x", app(var("y"), var("x")));
    ///
    /// expr.apply::<Enumerate>(&var("z"));
    ///
    /// assert_eq!(expr, app(var("y"), var("z")));
    /// ```
    pub fn apply<A>(&mut self, rhs: &Term)
    where
        A: AlphaRename,
    {
        apply_mut::<A>(self, rhs)
    }

    /// Performs a [β-reduction] on this `Term`.
    ///
    /// The reduction strategy to be used must be given as a type parameter,
    /// like in the example below.
    ///
    /// # Example
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate lamcal;
    /// # use lamcal::{var, lam, app, NormalOrder, Enumerate};
    /// # fn main() {
    /// let mut expr = app![
    ///     lam("a", var("a")),
    ///     lam("b", lam("c", var("b"))),
    ///     var("x"),
    ///     lam("e", var("f"))
    /// ];
    ///
    /// expr.reduce::<NormalOrder<Enumerate>>();
    ///
    /// assert_eq!(expr, var("x"));
    /// # }
    /// ```
    /// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
    pub fn reduce<B>(&mut self)
    where
        B: BetaReduce,
    {
        let expr = mem::replace(self, Var(String::new()));
        *self = <B as BetaReduce>::reduce(expr);
    }
}

/// Performs an [α-conversion] on a given lambda expression and returns the
/// result as a new `Term`.
///
/// The type parameter A defines the strategy to be used for renaming bound
/// variables.
///
/// The result is returned as a new `Term`. The original term `expr` is not
/// changed. If you want to perform an α-conversion on a term in place use the
/// associated function [`Term::alpha`](enum.Term.html#method.alpha) instead.
///
/// [α-conversion]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
pub fn alpha<A>(expr: &Term) -> Term
where
    A: AlphaRename,
{
    let mut expr = expr.clone();
    alpha_rec::<A>(&mut expr, Context::new());
    expr
}

fn alpha_rec<A>(expr: &mut Term, mut ctx: Context)
where
    A: AlphaRename,
{
    match *expr {
        Var(ref mut name) => if ctx.free.contains(name) && !ctx.bound.contains(name) {
            <A as AlphaRename>::rename(name);
        },
        Lam(VarT(ref mut name), ref mut body) => {
            if ctx.free.contains(name) {
                <A as AlphaRename>::rename(name);
            };
            ctx.bound.insert(name.to_owned());
            alpha_rec::<A>(body, ctx);
        },
        App(ref mut lhs, ref mut rhs) => {
            let traverse1 = if let Var(ref name1) = **lhs {
                !ctx.free.insert(name1.to_owned())
            } else {
                true
            };
            let traverse2 = if let Var(ref name2) = **rhs {
                !ctx.free.insert(name2.to_owned())
            } else {
                true
            };
            if traverse1 {
                alpha_rec::<A>(lhs, ctx.clone());
            }
            if traverse2 {
                alpha_rec::<A>(rhs, ctx);
            }
        },
    }
}

/// Defines a strategy for renaming variables during [α-conversion] of terms.
///
/// A possible implementations may choose the next letter in the alphabet for
/// single character names. Another strategy may be to enumerate the variables
/// by appending an increasing number. A third example for an implementation
/// is appending a tick symbol to the variable name.
///
/// [α-conversion]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
pub trait AlphaRename {
    /// Renames the given variable name according the implemented strategy.
    fn rename(name: &mut String);
}

/// Implementation of `AlphaRename` that appends an increasing number to the
/// name.
///
/// If the given name ends with a number this number is replaced by the number
/// increased by one. If the last character is a letter the digit 1 is appended.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Enumerate;

impl AlphaRename for Enumerate {
    fn rename(name: &mut String) {
        let digits = name
            .chars()
            .rev()
            .take_while(char::is_ascii_digit)
            .collect::<String>()
            .chars()
            .rev()
            .collect::<String>();
        // digits should be either a parsable number or an empty string
        if let Ok(number) = digits.parse::<usize>() {
            let index = name.len() - digits.len();
            name.drain(index..);
            name.push_str(&(number + 1).to_string());
        } else {
            name.push('1');
        }
    }
}

/// Implementation of `AlphaRename` that appends a tick symbol '\' at the end of
/// a variable name.
///
/// If the given name already ends with a tick symbol another tick symbol is
/// appended.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Prime;

impl AlphaRename for Prime {
    fn rename(name: &mut String) {
        name.push('\'');
    }
}

#[derive(Debug, Clone)]
struct Context {
    free: HashSet<String>,
    bound: HashSet<String>,
}

impl Context {
    fn new() -> Self {
        Context {
            free: HashSet::new(),
            bound: HashSet::new(),
        }
    }
}

/// Replaces all free occurrences of the variable `var` in the expression
/// `expr` with the expression `repl` and returns the resulting expression.
///
/// This function implements [substitution] in terms of the lambda calculus as
/// a recursion on the structure of the given `expr`.
///
/// The result is returned as a new `Term`. The original term `expr` is not
/// changed.
///
/// To avoid name clashes this function performs α-conversions when appropriate.
/// Therefore a strategy for α-conversion must be given as a type parameter.
///
/// This function returns the result as a new `Term`. The given term `expr`
/// remains unmodified. If you want to substitute the original term in place use
/// the associated function [`Term::subst`](enum.Term.html#method.subst)
/// instead.
///
/// [substitution]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
pub fn substitute<A>(expr: &Term, var: &VarT, repl: &Term) -> Term
where
    A: AlphaRename,
{
    let mut t_expr = app(expr.clone(), repl.clone());
    alpha_rec::<A>(&mut t_expr, Context::new());
    if let App(mut a_expr, _) = t_expr {
        substitute_rec(&mut a_expr, var, repl);
        *a_expr
    } else {
        unreachable!("we just created a Term::App before")
    }
}

fn substitute_rec(expr: &mut Term, var: &VarT, repl: &Term) {
    let subst = match *expr {
        Var(ref name) => name == var.as_ref(),
        Lam(ref mut param, ref mut body) => {
            if param != var {
                substitute_rec(body, var, repl);
            }
            false
        },
        App(ref mut lhs, ref mut rhs) => {
            substitute_rec(lhs, var, repl);
            substitute_rec(rhs, var, repl);
            false
        },
    };
    if subst {
        *expr = repl.clone();
    }
}

/// Applies a given lambda abstraction to the given substitution term and
/// returns the result as a new `Term`.
///
/// If the given term `expr` is a lambda abstraction (that is of variant
/// `Term::Lam`) any occurrence of its bound variable in its body is replaced by
/// the given term `subst`. The substitution is applied recursively.
///
/// The result is returned as a new `Term`. The original term `expr` is not
/// changed. If the given expression `expr` is not a lambda abstraction an
/// unmodified clone of the term `expr` is returned.
///
/// To avoid name clashes this function performs α-conversions when appropriate.
/// Therefore a strategy for α-conversion must be given as a type parameter.
///
/// This function returns the result as a new `Term`. The given term `expr`
/// remains unmodified. If you want to apply an α-conversion on the original
/// term in place use the associated function
/// [`Term::apply`](enum.Term.html#method.apply) instead.
///
/// # Example 1
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{app, lam, var, apply, Enumerate};
/// let expr1 = lam("x", app(var("x"), var("y")));
///
/// let expr2 = apply::<Enumerate>(&expr1, &var("z"));
///
/// assert_eq!(expr2, app(var("z"), var("y")));
/// ```
///
/// # Example 2
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{app, lam, var, apply, Enumerate};
/// let expr1 = app(var("x"), var("y"));
///
/// let expr2 = apply::<Enumerate>(&expr1, &var("z"));
///
/// assert_eq!(expr2, expr1);
/// ```
pub fn apply<A>(expr: &Term, subst: &Term) -> Term
where
    A: AlphaRename,
{
    let mut t_expr = app(expr.clone(), subst.clone());
    alpha_rec::<A>(&mut t_expr, Context::new());
    if let App(mut a_expr, _) = t_expr {
        apply_mut::<A>(&mut a_expr, subst);
        *a_expr
    } else {
        unreachable!("we just created a Term::App before")
    }
}

fn apply_mut<A>(expr: &mut Term, subst: &Term)
where
    A: AlphaRename,
{
    alpha_rec::<A>(expr, Context::new());
    if let Some(bound) = match *expr {
        Lam(ref param, _) => Some(param.as_ref().to_owned()),
        _ => None,
    } {
        apply_rec(expr, subst, &bound);
        if let Some(replace) = match *expr {
            Lam(_, ref mut body) => Some(mem::replace(&mut **body, Var(String::new()))),
            _ => None,
        } {
            *expr = replace;
        }
    }
}

fn apply_rec(expr: &mut Term, subst: &Term, bound: &str) {
    if let Some(replace) = match *expr {
        Var(ref name) => if name == bound {
            Some(subst.clone())
        } else {
            None
        },
        Lam(_, ref mut body) => {
            apply_rec(body, subst, bound);
            None
        },
        App(ref mut lhs, ref mut rhs) => {
            apply_rec(lhs, subst, bound);
            apply_rec(rhs, subst, bound);
            None
        },
    } {
        *expr = replace;
    }
}

/// Performs a β-reduction on a given lambda expression applying the given
/// reduction strategy.
///
/// The reduction strategy to be used must be given as a type parameter, like
/// in the example below.
///
/// This function returns the result as a new `Term`. The given `Term` remains
/// unchanged. If you want to apply a β-reduction modifying the term in place
/// use the associated function [`Term::reduce`](enum.Term.html#method.reduce)
/// instead.
///
/// # Example
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// # use lamcal::{var, lam, app, reduce, NormalOrder, Enumerate};
/// # fn main() {
/// let expr = app![
///     lam("a", var("a")),
///     lam("b", lam("c", var("b"))),
///     var("x"),
///     lam("e", var("f"))
/// ];
///
/// let reduced = reduce::<NormalOrder<Enumerate>>(&expr);
///
/// assert_eq!(reduced, var("x"));
/// # }
/// ```
pub fn reduce<B>(expr: &Term) -> Term
where
    B: BetaReduce,
{
    <B as BetaReduce>::reduce(expr.clone())
}

/// Defines a strategy for [β-reduction] of terms.
///
/// Possible implementations may follow the strategies described in the
/// [reduction strategy] article on wikipedia.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
/// [reduction strategy]: https://en.wikipedia.org/wiki/Reduction_strategy_(lambda_calculus)
pub trait BetaReduce {
    /// Performs a β-reduction of the given `Term` and returns the result.
    fn reduce(expr: Term) -> Term;
}

/// Implementation of a [β-reduction] applying the call-by-name strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct CallByName<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for CallByName<A>
where
    A: AlphaRename,
{
    /// Performs a β-reduction on a given lambda expression applying a
    /// call-by-name strategy.
    fn reduce(mut expr: Term) -> Term {
        alpha_rec::<A>(&mut expr, Context::new());
        CallByName::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> CallByName<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(replace) = match *expr {
            App(ref mut lhs, ref rhs) => {
                CallByName::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        CallByName::<A>::reduce_rec(lhs);
                        // delay replacement outside match expression because of the borrow checker
                        Some(mem::replace(&mut **lhs, Var(String::new())))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = replace;
        }
    }
}

/// Implementation of a [β-reduction] applying the normal-order strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct NormalOrder<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for NormalOrder<A>
where
    A: AlphaRename,
{
    /// Performs a β-reduction on a given lambda expression applying a
    /// normal-order strategy.
    fn reduce(mut expr: Term) -> Term {
        alpha_rec::<A>(&mut expr, Context::new());
        NormalOrder::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> NormalOrder<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(replace) = match *expr {
            Lam(_, ref mut body) => {
                NormalOrder::<A>::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                CallByName::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        NormalOrder::<A>::reduce_rec(lhs);
                        // delay replacement outside match expression because of the borrow checker
                        Some(mem::replace(&mut **lhs, Var(String::new())))
                    },
                    _ => {
                        NormalOrder::<A>::reduce_rec(lhs);
                        NormalOrder::<A>::reduce_rec(rhs);
                        None
                    },
                }
            },
            _ => None,
        } {
            *expr = replace;
        }
    }
}
