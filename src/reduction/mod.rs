#[cfg(test)]
mod tests;

use std::collections::HashSet;

use term::{Term, Var as VarT};

/// Performs an α-reduction on a given lambda expression.
///
/// When called with an owned `Expr` the given expression is modified in place
/// and returned again. When called with a reference to an `Expr` a cloned
/// expression with the conversions applied is returned.
pub fn convert(expr: impl Into<Term>) -> Term {
    let mut expr = expr.into();
    traverse_expression(&mut expr, Context::new());
    expr
}

/// Examples:
///
/// (λy.λx.x y) x  =>  (λy.λx1.x1 y) x
/// (λy.y x) x  =>  (λy.y x1) x
/// (λy.y x)(λy.y x)  =>  (λy.y x)(λy.y x)
fn traverse_expression(expr: &mut Term, mut ctx: Context) {
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
            traverse_expression(body, ctx);
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
                traverse_expression(expr1, ctx.clone());
            }
            if traverse2 {
                traverse_expression(expr2, ctx);
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

/// Performs a β-reduction on a given lambda expression.
pub fn reduce(expr: impl Into<Term>) -> Term {
    unimplemented!()
}
