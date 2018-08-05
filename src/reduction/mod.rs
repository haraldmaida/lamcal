#[cfg(test)]
mod tests;

use std::collections::HashSet;

use term::{app, Term, Var as VarT};

/// Performs an α-reduction on a given lambda expression.
///
/// When called with an owned `Expr` the given expression is modified in place
/// and returned again. When called with a reference to an `Expr` a cloned
/// expression with the conversions applied is returned.
pub fn convert(expr: impl Into<Term>) -> Term {
    let mut expr = expr.into();
    convert_rec(&mut expr, Context::new());
    expr
}

/// Examples:
///
/// (λy.λx.x y) x  =>  (λy.λx1.x1 y) x
/// (λy.y x) x  =>  (λy.y x1) x
/// (λy.y x)(λy.y x)  =>  (λy.y x)(λy.y x)
fn convert_rec(expr: &mut Term, mut ctx: Context) {
    use self::Term::*;
    match *expr {
        Var(ref mut name) => if ctx.free.contains(name) && !ctx.bound.contains(name) {
            alpha_convert(name);
        },
        Lam(VarT(ref mut name), ref mut body) => {
            if ctx.free.contains(name) {
                alpha_convert(name);
            };
            ctx.bound.insert(name.to_owned());
            convert_rec(body, ctx);
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
                convert_rec(expr1, ctx.clone());
            }
            if traverse2 {
                convert_rec(expr2, ctx);
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
/// When `expr` is an owned `Expr` the given expression is modified in place
/// and returned again. When called with a reference to an `Expr` a cloned
/// expression with the substitution applied is returned.
///
/// [substitution]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
pub fn substitute(expr: impl Into<Term>, var: &VarT, repl: &Term) -> Term {
    if let Term::App(mut a_expr, _) = convert(app(expr.into(), var.clone().into())) {
        substitute_rec(&mut a_expr, var, repl);
        *a_expr
    } else {
        unreachable!("we just created a Term::App before")
    }
}

fn substitute_rec(expr: &mut Term, var: &VarT, repl: &Term) {
    use self::Term::*;
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
/// When `expr` is an owned `Expr` the given expression is modified in place
/// and returned again. When called with a reference to an `Expr` a cloned
/// expression with the substitution applied is returned.
pub fn apply(expr: impl Into<Term>, subst: &Term) -> Term {
    if let Term::App(mut a_expr, _) = convert(app(expr.into(), subst.clone().into())) {
        apply_rec(&mut a_expr, subst, "");
        *a_expr
    } else {
        unreachable!("we just created a Term::App before")
    }
}

fn apply_rec(expr: &mut Term, subst: &Term, bound: &str) {
    use self::Term::*;
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

/// Performs a β-reduction on a given lambda expression.
pub fn reduce(expr: impl Into<Term>) -> Term {
    unimplemented!()
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CallByName;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NormalOrder;
