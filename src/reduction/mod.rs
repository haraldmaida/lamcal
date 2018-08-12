//!
//! [evaluation strategies]: https://en.wikipedia.org/wiki/Evaluation_strategy
//! [reduction strategies]: https://en.wikipedia.org/wiki/Lambda_calculus#Reduction_strategies

#[cfg(test)]
mod tests;

use std::collections::HashSet;
use std::marker::PhantomData;

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
            match *self {
                App(ref expr1, ref expr2) => expr1.is_beta_normal() && expr2.is_beta_normal(),
                _ => true,
            }
        }
    }

    /// Performs an [α-reduction] on this `Term`.
    ///
    /// [α-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
    pub fn alpha<R>(&mut self)
    where
        R: AlphaRename,
    {
        alpha_rec::<R>(self, Context::new())
    }

    /// [Substitutes] all occurrences of `var` as free variables with the
    /// expression `rhs` recursively on the structure of this `Term`.
    ///
    /// [substitutes]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
    pub fn subst(&mut self, var: &VarT, rhs: &Term) {
        substitute_rec(self, var, rhs)
    }

    /// Applies the expression `rhs` to the param of this lambda abstraction if
    /// this `Term` is of variant `Term::Lam` by recursively substituting all
    /// occurrences of the bound variable in the body of the lambda abstraction
    /// with the expression `rhs`.
    ///
    /// If this `Term` is not a lambda abstraction this function does nothing.
    pub fn apply(&mut self, rhs: &Term) {
        apply_rec(self, rhs, "")
    }

    /// Performs a [β-reduction] on this `Term`.
    ///
    /// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
    pub fn reduce(&mut self) {
        NormalOrder::<Enumerate>::reduce_rec(self, "")
    }
}

/// Performs an α-reduction on a given lambda expression.
///
/// When called with an owned `Term` the given expression is modified in place
/// and returned again. When called with a reference to a `Term` a cloned
/// expression with the conversions applied is returned.
pub fn alpha<R, T>(expr: T) -> Term
where
    R: AlphaRename,
    T: Into<Term>,
{
    let mut expr = expr.into();
    alpha_rec::<R>(&mut expr, Context::new());
    expr
}

/// Examples:
///
/// (λy.λx.x y) x  =>  (λy.λx1.x1 y) x
/// (λy.y x) x  =>  (λy.y x1) x
/// (λy.y x)(λy.y x)  =>  (λy.y x)(λy.y x)
fn alpha_rec<R>(expr: &mut Term, mut ctx: Context)
where
    R: AlphaRename,
{
    match *expr {
        Var(ref mut name) => if ctx.free.contains(name) && !ctx.bound.contains(name) {
            <R as AlphaRename>::rename(name);
        },
        Lam(VarT(ref mut name), ref mut body) => {
            if ctx.free.contains(name) {
                <R as AlphaRename>::rename(name);
            };
            ctx.bound.insert(name.to_owned());
            alpha_rec::<R>(body, ctx);
        },
        App(ref mut expr1, ref mut expr2) => {
            let traverse1 = if let &Var(ref name1) = &**expr1 {
                !ctx.free.insert(name1.to_owned())
            } else {
                true
            };
            let traverse2 = if let &Var(ref name2) = &**expr2 {
                !ctx.free.insert(name2.to_owned())
            } else {
                true
            };
            if traverse1 {
                alpha_rec::<R>(expr1, ctx.clone());
            }
            if traverse2 {
                alpha_rec::<R>(expr2, ctx);
            }
        },
    }
}

/// Defines a strategy for renaming variables during [α-reduction] of terms.
///
/// A possible implementations may choose the next letter in the alphabet for
/// single character names. Another strategy may be to enumerate the variables
/// by appending an increasing number. A third example for an implementation
/// is appending a tick symbol to the variable name.
///
/// [α-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
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
            name.extend((number + 1).to_string().chars());
        } else {
            name.push('1');
        }
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
/// When `expr` is an owned `Term` the given expression is modified in place
/// and returned again. When called with a reference to a `Term` a cloned
/// expression with the substitution applied is returned.
///
/// [substitution]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
pub fn substitute<R>(expr: impl Into<Term>, var: &VarT, repl: &Term) -> Term
where
    R: AlphaRename,
{
    let mut t_expr = app(expr.into(), repl.clone().into());
    alpha_rec::<R>(&mut t_expr, Context::new());
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
        App(ref mut expr1, ref mut expr2) => {
            substitute_rec(expr1, var, repl);
            substitute_rec(expr2, var, repl);
            false
        },
    };
    if subst {
        *expr = repl.clone();
    }
}

/// Applies the expression `subst` to the expression `expr` if `expr` is a
/// lambda abstraction (that it is of variant `Expr::Lam`) and returns the
/// resulting expression.
///
/// In the result any occurrence of the bound variable of the lambda abstraction
/// is substituted by the given expression `subst` in the body of the lambda
/// abstraction recursively.
///
/// If the given expression `expr` is not a lambda abstraction the expression is
/// returned unmodified.
///
/// When `expr` is an owned `Term` the given expression is modified in place
/// and returned again. When called with a reference to a `Term` a cloned
/// expression with the substitution applied is returned.
pub fn apply<R>(expr: impl Into<Term>, subst: &Term) -> Term
where
    R: AlphaRename,
{
    let mut t_expr = app(expr.into(), subst.clone().into());
    alpha_rec::<R>(&mut t_expr, Context::new());
    if let App(mut a_expr, _) = t_expr {
        apply_rec(&mut a_expr, subst, "");
        *a_expr
    } else {
        unreachable!("we just created a Term::App before")
    }
}

fn apply_rec(expr: &mut Term, subst: &Term, bound: &str) {
    let apply = match *expr {
        Var(ref name) => name == bound,
        Lam(ref param, ref mut body) => {
            apply_rec(body, subst, param.as_ref());
            false
        },
        App(ref mut expr1, ref mut expr2) => {
            apply_rec(expr1, subst, bound);
            apply_rec(expr2, subst, bound);
            false
        },
    };
    if apply {
        *expr = subst.clone();
    }
}

/// Performs a β-reduction on a given lambda expression applying the given
/// reduction strategy.
///
/// When `expr` is an owned `Term` the given expression is modified in place
/// and returned again. When called with a reference to a `Term` a cloned
/// expression with the reduction applied is returned.
pub fn reduce<S>(expr: impl Into<Term>) -> Term
where
    S: BetaReduce,
{
    <S as BetaReduce>::reduce(expr)
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
    ///
    /// When `expr` is an owned `Term` the given expression is modified in place
    /// and returned again. When called with a reference to a `Term` a cloned
    /// expression with the reduction applied is returned.
    fn reduce(expr: impl Into<Term>) -> Term;
}

/// Implementation of a [β-reduction] applying the call-by-name strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct CallByName<R> {
    _alpha_rename: PhantomData<R>,
}

impl<R> BetaReduce for CallByName<R>
where
    R: AlphaRename,
{
    /// Performs a β-reduction on a given lambda expression applying a
    /// call-by-name strategy.
    fn reduce(expr: impl Into<Term>) -> Term {
        let mut b_term = expr.into();
        alpha_rec::<R>(&mut b_term, Context::new());
        CallByName::<R>::reduce_rec(&mut b_term, "");
        b_term
    }
}

impl<R> CallByName<R> {
    fn reduce_rec(expr: &mut Term, bound: &str) {
        match *expr {
            App(ref mut expr1, ref expr2) => {
                CallByName::<R>::reduce_rec(expr1, bound);
                if expr1.is_beta_redex() {
                    apply_rec(expr1, expr2, bound);
                    CallByName::<R>::reduce_rec(expr1, bound);
                }
            },
            _ => {},
        }
    }
}

/// Implementation of a [β-reduction] applying the normal-order strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct NormalOrder<R> {
    _alpha_rename: PhantomData<R>,
}

impl<R> BetaReduce for NormalOrder<R>
where
    R: AlphaRename,
{
    /// Performs a β-reduction on a given lambda expression applying a
    /// normal-order strategy.
    fn reduce(expr: impl Into<Term>) -> Term {
        let mut b_term = expr.into();
        alpha_rec::<R>(&mut b_term, Context::new());
        NormalOrder::<R>::reduce_rec(&mut b_term, "");
        b_term
    }
}

impl<R> NormalOrder<R> {
    fn reduce_rec(expr: &mut Term, bound: &str) {
        match *expr {
            Lam(ref param, ref mut body) => NormalOrder::<R>::reduce_rec(body, param.as_ref()),
            App(ref mut expr1, ref mut expr2) => {
                CallByName::<R>::reduce_rec(expr1, bound);
                if expr1.is_beta_redex() {
                    apply_rec(expr1, expr2, bound);
                    NormalOrder::<R>::reduce_rec(expr1, bound);
                } else {
                    NormalOrder::<R>::reduce_rec(expr1, bound);
                    NormalOrder::<R>::reduce_rec(expr2, bound);
                }
            },
            _ => {},
        }
    }
}
