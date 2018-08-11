//!
//! [evaluation strategies]: https://en.wikipedia.org/wiki/Evaluation_strategy
//! [reduction strategies]: https://en.wikipedia.org/wiki/Lambda_calculus#Reduction_strategies

#[cfg(test)]
mod tests;

use std::collections::HashSet;

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
    pub fn alpha(&mut self) {
        alpha_rec(self, Context::new())
    }

    /// [Substitutes] all occurrences `var` as free variables with the
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
        unimplemented!()
    }
}

/// Performs an α-reduction on a given lambda expression.
///
/// When called with an owned `Term` the given expression is modified in place
/// and returned again. When called with a reference to a `Term` a cloned
/// expression with the conversions applied is returned.
pub fn alpha(expr: impl Into<Term>) -> Term {
    let mut expr = expr.into();
    alpha_rec(&mut expr, Context::new());
    expr
}

/// Examples:
///
/// (λy.λx.x y) x  =>  (λy.λx1.x1 y) x
/// (λy.y x) x  =>  (λy.y x1) x
/// (λy.y x)(λy.y x)  =>  (λy.y x)(λy.y x)
fn alpha_rec(expr: &mut Term, mut ctx: Context) {
    match *expr {
        Var(ref mut name) => if ctx.free.contains(name) && !ctx.bound.contains(name) {
            alpha_convert(name);
        },
        Lam(VarT(ref mut name), ref mut body) => {
            if ctx.free.contains(name) {
                alpha_convert(name);
            };
            ctx.bound.insert(name.to_owned());
            alpha_rec(body, ctx);
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
                alpha_rec(expr1, ctx.clone());
            }
            if traverse2 {
                alpha_rec(expr2, ctx);
            }
        },
    }
}

fn alpha_convert(name: &mut String) {
    let (index, last) = name
        .chars()
        .enumerate()
        .last()
        .expect("A name should never be empty");
    if let Some(digit) = last.to_digit(10) {
        name.remove(index);
        name.push_str(&(digit + 1).to_string());
    } else {
        name.push('1');
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
pub fn substitute(expr: impl Into<Term>, var: &VarT, repl: &Term) -> Term {
    let mut t_expr = app(expr.into(), repl.clone().into());
    alpha_rec(&mut t_expr, Context::new());
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
pub fn apply(expr: impl Into<Term>, subst: &Term) -> Term {
    let mut t_expr = app(expr.into(), subst.clone().into());
    alpha_rec(&mut t_expr, Context::new());
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

/// Defines a [reduction strategy] for β-reduction of terms.
///
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
pub struct CallByName;

impl BetaReduce for CallByName {
    /// Performs a β-reduction on a given lambda expression applying a
    /// call-by-name strategy.
    fn reduce(expr: impl Into<Term>) -> Term {
        let mut b_term = expr.into();
        alpha_rec(&mut b_term, Context::new());
        CallByName::reduce_rec(&mut b_term, "");
        b_term
    }
}

impl CallByName {
    fn reduce_rec(expr: &mut Term, bound: &str) {
        match *expr {
            App(ref mut expr1, ref expr2) => {
                CallByName::reduce_rec(expr1, bound);
                if expr1.is_beta_redex() {
                    apply_rec(expr1, expr2, bound);
                    CallByName::reduce_rec(expr1, bound);
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
pub struct NormalOrder;

impl BetaReduce for NormalOrder {
    /// Performs a β-reduction on a given lambda expression applying a
    /// normal-order strategy.
    fn reduce(expr: impl Into<Term>) -> Term {
        let mut b_term = expr.into();
        alpha_rec(&mut b_term, Context::new());
        NormalOrder::reduce_rec(&mut b_term, "");
        b_term
    }
}

impl NormalOrder {
    fn reduce_rec(expr: &mut Term, bound: &str) {
        match *expr {
            Lam(ref param, ref mut body) => NormalOrder::reduce_rec(body, param.as_ref()),
            App(ref mut expr1, ref mut expr2) => {
                CallByName::reduce_rec(expr1, bound);
                if expr1.is_beta_redex() {
                    apply_rec(expr1, expr2, bound);
                    NormalOrder::reduce_rec(expr1, bound);
                } else {
                    NormalOrder::reduce_rec(expr1, bound);
                    NormalOrder::reduce_rec(expr2, bound);
                }
            },
            _ => {},
        }
    }
}
