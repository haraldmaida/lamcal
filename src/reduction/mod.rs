//! Reduction system of the lambda calculus.
//!
//! [evaluation strategies]: https://en.wikipedia.org/wiki/Evaluation_strategy
//! [reduction strategies]: https://en.wikipedia.org/wiki/Lambda_calculus#Reduction_strategies

#[cfg(test)]
mod tests;

use std::collections::HashSet;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::mem;

use environment::Environment;
use term::{Term, Term::*, VarName};

fn dummy_term() -> Term {
    Var(VarName(String::new()))
}

impl Term {
    /// Returns whether this `Term` is a [beta redex].
    ///
    /// A beta redex is a term of the form (λx.A) M
    ///
    /// The term redex, short for reducible expression, refers to subterms that
    /// can be reduced by one of the reduction rules.
    ///
    /// # Examples
    ///
    /// ```
    /// # use lamcal::{var, lam, app};
    /// let expr1 = app(lam("a", var("a")), var("x"));
    ///
    /// assert!(expr1.is_beta_redex());
    ///
    /// let expr2 = app(var("x"), lam("a", var("a")));
    ///
    /// assert!(!expr2.is_beta_redex());
    ///
    /// let expr3 = app(var("x"), app(lam("a", var("a")), var("y")));
    ///
    /// assert!(expr3.is_beta_redex());
    /// ```
    ///
    /// [beta redex]: https://en.wikipedia.org/wiki/Beta_normal_form#Beta_reduction
    pub fn is_beta_redex(&self) -> bool {
        let mut to_check = Vec::with_capacity(16);
        to_check.push(self);
        while let Some(term) = to_check.pop() {
            match *term {
                Var(_) => {},
                Lam(_, ref body) => to_check.push(body),
                App(ref lhs, ref rhs) => match **lhs {
                    Lam(_, _) => return true,
                    _ => {
                        to_check.push(rhs);
                        to_check.push(lhs);
                    },
                },
            }
        }
        return false;
    }

    /// Returns whether this `Term` is a [beta normal form].
    ///
    /// A beta normal form is a term that does not contain any beta redex,
    /// i.e. that cannot be further reduced.
    ///
    /// [beta normal form]: https://en.wikipedia.org/wiki/Beta_normal_form
    pub fn is_beta_normal(&self) -> bool {
        !self.is_beta_redex()
    }

    /// Performs an [α-conversion] on this `Term`.
    ///
    /// [α-conversion]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
    pub fn alpha<A>(&mut self, names: &HashSet<&VarName>)
    where
        A: AlphaRename,
    {
        let names = HashSet::from_iter(
            names
                .into_iter()
                .cloned()
                .cloned()
                .chain(self.free_vars().into_iter().cloned()),
        );
        alpha_rec::<A>(self, &names, HashSet::new())
    }

    /// [Substitutes] all occurrences of `var` as free variables with the
    /// expression `rhs` recursively on the structure of this `Term`.
    ///
    /// [substitutes]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
    pub fn substitute(&mut self, var: &VarName, rhs: &Term) {
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
    /// appropriate. Therefore a strategy for α-conversion must be given as the
    /// type parameter `A`.
    ///
    /// # Examples
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
    /// The reduction strategy to be used must be given as the type parameter
    /// `B`, like in the example below.
    ///
    /// # Examples
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
    ///
    /// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
    pub fn reduce<B>(&mut self)
    where
        B: BetaReduce,
    {
        let expr = mem::replace(self, dummy_term());
        *self = <B as BetaReduce>::reduce(expr);
    }

    /// Replaces free variables with the term bound to the variable's name in
    /// the given environment.
    ///
    /// This method walks through this whole term and replaces any variable
    /// with the term bound to the variable's name in the given environment.
    /// Bound variables are not replaced.
    ///
    /// This method modifies this `Term` in place. If you want to expand named
    /// constants and get the result as a new `Term` while keeping the original
    /// `Term` unchanged use the standalone function [`expand`](fn.expand.html)
    /// instead.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate lamcal;
    /// # use lamcal::{app, expand, lam, var, Environment};
    /// # fn main() {
    /// let env = Environment::default();
    ///
    /// let mut expr = app![
    ///     var("C"),
    ///     lam("a", app(var("K"), var("I"))),
    ///     var("e"),
    ///     var("f")
    /// ];
    ///
    /// expr.expand(&env);
    ///
    /// assert_eq!(
    ///     expr,
    ///     app![
    ///         lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
    ///         lam("a", app(lam("a", lam("b", var("a"))), lam("a", var("a")))),
    ///         var("e"),
    ///         var("f")
    ///     ]
    /// );
    /// # }
    /// ```
    pub fn expand(&mut self, env: &Environment) {
        expand_rec(self, HashSet::new(), env)
    }

    /// Evaluates this lambda expression in the given environment.
    ///
    /// Evaluation comprises the following steps in the given order:
    ///
    /// * expand all named constants with their bound terms found in the
    ///   environment recursively
    /// * perform β-reduction on the expression
    /// * perform α-conversion where needed to avoid name clashes
    ///
    /// For the β-reduction step a reduction strategy is required. Therefore the
    /// reduction strategy must be specified as the type parameter `B`.
    ///
    /// The expansion of named constants step as done by this function is
    /// equivalent to calling the
    /// [`Term::expand`](enum.Term.html#method.expand) method. Similar the
    /// β-reduction step performed by this the function is equivalent to calling
    /// the [`Term::reduce`](enum.Term.html#method.reduce) method.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate lamcal;
    /// # use lamcal::{app, evaluate, lam, var, Enumerate, Environment, NormalOrder};
    /// # fn main() {
    /// let env = Environment::default();
    ///
    /// let mut expr = app![
    ///     var("C"),
    ///     lam("x", lam("y", app(var("x"), var("y")))),
    ///     var("e"),
    ///     var("f")
    /// ];
    ///
    /// expr.evaluate::<NormalOrder<Enumerate>>(&env);
    ///
    /// assert_eq!(expr, app(var("f"), var("e")));
    /// # }
    /// ```
    pub fn evaluate<B>(&mut self, env: &Environment)
    where
        B: BetaReduce,
    {
        self.expand(env);
        self.reduce::<B>();
    }
}

/// Evaluates a lambda expression in the given environment.
///
/// This function takes the given expression by reference and returns a new
/// `Term` with the evaluation applied. The given `Term` remains unchanged.
///
/// Evaluation comprises the following steps in the given order:
///
/// * expand all named constants with their bound terms found in the
///   environment recursively
/// * perform β-reduction on the expression
/// * perform α-conversion where needed to avoid name clashes
///
/// For the β-reduction step a reduction strategy is required. Therefore the
/// reduction strategy must be specified as the type parameter `B`.
///
/// The expansion of named constants step as done by this function is
/// equivalent to calling the [`expand`](fn.expand.html) function. Similar the
/// β-reduction step performed by this the function is equivalent to calling
/// the [`reduce`](fn.reduce.html) function.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// # use lamcal::{app, evaluate, lam, var, Enumerate, Environment, NormalOrder};
/// # fn main() {
/// let env = Environment::default();
///
/// let expr = app![
///     var("C"),
///     lam("x", lam("y", app(var("x"), var("y")))),
///     var("e"),
///     var("f")
/// ];
///
/// let result = evaluate::<NormalOrder<Enumerate>>(&expr, &env);
///
/// assert_eq!(result, app(var("f"), var("e")));
/// # }
/// ```
pub fn evaluate<B>(expr: &Term, env: &Environment) -> Term
where
    B: BetaReduce,
{
    let mut expr2 = expr.clone();
    expr2.evaluate::<B>(env);
    expr2
}

/// Replaces free variables in a term with the term that is bound to the
/// variable's name in the given environment.
///
/// This function walks through the whole term and replaces any free variable
/// with the term bound to the variable's name in the given environment. Bound
/// variables are not replaced.
///
/// The result is returned as a new `Term`. The given term remains unchanged. If
/// you want to expand named constants in a term in place use the associated
/// function [`Term::expand`](enum.Term.html#method.expand) instead.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// # use lamcal::{app, expand, lam, var, Environment};
/// # fn main() {
/// let env = Environment::default();
///
/// let expr = app![
///     var("C"),
///     lam("a", app(var("K"), var("I"))),
///     var("e"),
///     var("f")
/// ];
///
/// let result = expand(&expr, &env);
///
/// assert_eq!(
///     result,
///     app![
///         lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
///         lam("a", app(lam("a", lam("b", var("a"))), lam("a", var("a")))),
///         var("e"),
///         var("f")
///     ]
/// );
/// # }
/// ```
///
/// Bound variables are not replaced even though there is a bound term for the
/// identifier `K` defined in the environment:
///
/// ```
/// # extern crate lamcal;
/// # use lamcal::{app, expand, lam, var, Environment};
/// # fn main() {
/// let env = Environment::default();
///
/// let expr = lam("K", app(var("K"), var("I")));
///
/// let result = expand(&expr, &env);
///
/// assert_eq!(result, lam("K", app(var("K"), lam("a", var("a")))));
/// # }
/// ```
pub fn expand(expr: &Term, env: &Environment) -> Term {
    let mut expr2 = expr.clone();
    expand_rec(&mut expr2, HashSet::new(), env);
    expr2
}

fn expand_rec(expr: &mut Term, mut bound_vars: HashSet<VarName>, env: &Environment) {
    let maybe_expand_with = match *expr {
        Var(ref name) => if !bound_vars.contains(name) {
            match env.lookup_term(name).cloned() {
                Some(mut expansion) => {
                    expand_rec(&mut expansion, bound_vars, env);
                    Some(expansion)
                },
                None => None,
            }
        } else {
            None
        },
        Lam(ref param, ref mut body) => {
            bound_vars.insert(param.to_owned());
            expand_rec(body, bound_vars, env);
            None
        },
        App(ref mut lhs, ref mut rhs) => {
            expand_rec(lhs, bound_vars.clone(), env);
            expand_rec(rhs, bound_vars, env);
            None
        },
    };
    if let Some(expand_with) = maybe_expand_with {
        *expr = expand_with;
    }
}

/// Performs an [α-conversion] on a given lambda expression and returns the
/// result as a new `Term`.
///
/// The type parameter `A` defines the strategy to be used for renaming bound
/// variables.
///
/// The result is returned as a new `Term`. The original term `expr` is not
/// changed. If you want to perform an α-conversion on a term in place use the
/// associated function [`Term::alpha`](enum.Term.html#method.alpha) instead.
///
/// [α-conversion]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
pub fn alpha<A>(expr: &Term, names: &HashSet<&VarName>) -> Term
where
    A: AlphaRename,
{
    let free_vars = HashSet::from_iter(
        names
            .into_iter()
            .cloned()
            .cloned()
            .chain(expr.free_vars().into_iter().cloned()),
    );
    let mut expr2 = expr.clone();
    alpha_rec::<A>(&mut expr2, &free_vars, HashSet::new());
    expr2
}

fn alpha_rec<A>(expr: &mut Term, free_vars: &HashSet<VarName>, mut bound_vars: HashSet<VarName>)
where
    A: AlphaRename,
{
    match *expr {
        Var(ref mut name) => if bound_vars.contains(name) {
            while free_vars.contains(name) {
                <A as AlphaRename>::rename(&mut **name);
            }
        },
        Lam(ref mut name, ref mut body) => {
            bound_vars.insert(name.to_owned());
            while free_vars.contains(name) {
                <A as AlphaRename>::rename(&mut **name);
            }
            alpha_rec::<A>(body, free_vars, bound_vars);
        },
        App(ref mut lhs, ref mut rhs) => {
            alpha_rec::<A>(lhs, free_vars, bound_vars.clone());
            alpha_rec::<A>(rhs, free_vars, bound_vars);
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

/// Implementation of `AlphaRename` that appends a tick symbol `'` at the end
/// of a variable name.
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

/// Replaces all free occurrences of the variable `var` in the expression
/// `expr` with the expression `subst` and returns the resulting expression.
///
/// This function implements [substitution] in terms of the lambda calculus as
/// a recursion on the structure of the given `expr`.
///
/// The result is returned as a new `Term`. The original term `expr` is not
/// changed.
///
/// To avoid name clashes this function performs α-conversions when appropriate.
/// Therefore a strategy for α-conversion must be specified as the type
/// parameter `A`.
///
/// This function returns the result as a new `Term`. The given term `expr`
/// remains unmodified. If you want to substitute the original term in place use
/// the associated function [`Term::subst`](enum.Term.html#method.subst)
/// instead.
///
/// [substitution]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
pub fn substitute<A>(expr: &Term, var: &VarName, subst: &Term) -> Term
where
    A: AlphaRename,
{
    let free_vars = HashSet::from_iter(
        subst
            .free_vars()
            .into_iter()
            .cloned()
            .chain(expr.free_vars().into_iter().cloned()),
    );
    let mut expr2 = expr.clone();
    alpha_rec::<A>(&mut expr2, &free_vars, HashSet::new());
    substitute_rec(&mut expr2, var, subst);
    expr2
}

fn substitute_rec(expr: &mut Term, var: &VarName, subst: &Term) {
    let do_subst = match *expr {
        Var(ref name) => name == var,
        Lam(ref mut param, ref mut body) => {
            if param != var {
                substitute_rec(body, var, subst);
            }
            false
        },
        App(ref mut lhs, ref mut rhs) => {
            substitute_rec(lhs, var, subst);
            substitute_rec(rhs, var, subst);
            false
        },
    };
    if do_subst {
        *expr = subst.clone();
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
/// Therefore a strategy for α-conversion must be specified as a type parameter
/// `A`.
///
/// This function returns the result as a new `Term`. The given term `expr`
/// remains unmodified. If you want to apply an α-conversion on the original
/// term in place use the associated function
/// [`Term::apply`](enum.Term.html#method.apply) instead.
///
/// # Examples
///
/// In the first example a lambda abstraction is applied to a variable z:
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
/// In this example a function application is applied to the variable z. Due
/// to an application can not be applied to a variable, the returned expression
/// is the same as the input expression:
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
    let free_vars = HashSet::from_iter(
        subst
            .free_vars()
            .into_iter()
            .cloned()
            .chain(expr.free_vars().into_iter().cloned()),
    );
    let expr2 = expr.clone();
    if let Lam(param, mut body) = expr2 {
        alpha_rec::<A>(&mut body, &free_vars, HashSet::new());
        substitute_rec(&mut body, &param, subst);
        *body
    } else {
        expr2
    }
}

fn apply_mut<A>(expr: &mut Term, subst: &Term)
where
    A: AlphaRename,
{
    let free_vars = HashSet::from_iter(
        subst
            .free_vars()
            .into_iter()
            .cloned()
            .chain(expr.free_vars().into_iter().cloned()),
    );
    if let Some(replace) = if let Some((param, body)) = expr.unwrap_lam_mut() {
        alpha_rec::<A>(body, &free_vars, HashSet::new());
        substitute_rec(body, param, subst);
        Some(mem::replace(body, dummy_term()))
    } else {
        None
    } {
        *expr = replace;
    }
}

/// Performs a [β-reduction] on a given lambda expression applying the given
/// reduction strategy.
///
/// The reduction strategy to be used must be given as the type parameter `B`,
/// like in the example below.
///
/// This function returns the result as a new `Term`. The given `Term` remains
/// unchanged. If you want to apply a β-reduction modifying the term in place
/// use the associated function [`Term::reduce`](enum.Term.html#method.reduce)
/// instead.
///
/// # Examples
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
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
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
    /// Performs β-reduction on the given `Term` and returns the result.
    fn reduce(expr: Term) -> Term;
}

/// Call-By-Name [β-reduction] to weak head normal form.
///
/// * Reduces the leftmost outermost redex not inside a lambda abstraction
///   first.
/// * It treats free variables as non-strict data constructors.
/// * Only leftmost redexes are contracted.
/// * No reduction is performed under abstractions.
///
/// This strategy is uniform as its definition involves no other reduction
/// strategy.
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
    /// Performs β-reduction on a given lambda expression applying a
    /// call-by-name strategy.
    fn reduce(mut expr: Term) -> Term {
        CallByName::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> CallByName<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
            App(ref mut lhs, ref rhs) => {
                CallByName::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        CallByName::<A>::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst;
        }
    }
}

/// Normal-Order [β-reduction] to normal form.
///
/// * Reduces the leftmost outermost redex first.
/// * In an application (e1 e2) the function term e1 is reduced using the
///   call-by-name strategy ([`CallByName`](struct.CallByName.html)).
/// * Any redex that is contracted is the leftmost one not contained in any
///   other redex.
/// * Reductions are performed also under lambda abstractions.
///
/// This strategy is a hybrid as it uses call-by-name for the reduction of the
/// expression e1 in function position in applications (e1 e2).
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
    /// Performs β-reduction on a given lambda expression applying a
    /// normal-order strategy.
    fn reduce(mut expr: Term) -> Term {
        NormalOrder::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> NormalOrder<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
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
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
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
            *expr = subst;
        }
    }
}

/// Call-By-Value [β-reduction] to weak normal form.
///
/// * Reduces the leftmost innermost redex not inside a lambda abstraction
///   first.
/// * It treats free variables as strict data constructors.
/// * An argument e2 of an application (e1 e2) is reduced before contracting
///   the redex and before building an application term.
/// * No reduction is performed under abstractions.
///
/// This strategy is uniform as its definition involves no other reduction
/// strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct CallByValue<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for CallByValue<A>
where
    A: AlphaRename,
{
    /// Performs β-reduction on a given lambda expression applying a
    /// call-by-value strategy.
    fn reduce(mut expr: Term) -> Term {
        CallByValue::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> CallByValue<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
            App(ref mut lhs, ref mut rhs) => {
                CallByValue::<A>::reduce_rec(lhs);
                CallByValue::<A>::reduce_rec(rhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        CallByValue::<A>::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst;
        }
    }
}

/// Applicative-Order [β-reduction] to normal form.
///
/// * Reduces the leftmost innermost redex first.
/// * It treats free variables as strict data constructors.
/// * An argument e2 of an application (e1 e2) is reduced before contracting
///   the redex and before building an application term.
/// * Reductions are performed also under lambda abstractions.
///
/// This strategy is uniform as its definition involves no other reduction
/// strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct ApplicativeOrder<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for ApplicativeOrder<A>
where
    A: AlphaRename,
{
    /// Performs β-reduction on a given lambda expression applying a
    /// applicative-order strategy.
    fn reduce(mut expr: Term) -> Term {
        ApplicativeOrder::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> ApplicativeOrder<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
            Lam(_, ref mut body) => {
                ApplicativeOrder::<A>::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                ApplicativeOrder::<A>::reduce_rec(lhs);
                ApplicativeOrder::<A>::reduce_rec(rhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        ApplicativeOrder::<A>::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst;
        }
    }
}

/// Hybrid-Applicative-Order [β-reduction] to normal form.
///
/// * A hybrid of call-by-value and applicative-order reduction.
/// * Reduces to normal form, but reduces under lambda abstractions only in
///   argument positions.
/// * Normalizes more terms than applicative-order reduction, while using fewer
///   reduction steps than normal order reduction.
///
/// The hybrid applicative order strategy relates to call-by-value in the
/// same way that the normal order strategy relates to call-by-name.
///
/// This strategy is a hybrid as it uses call-by-value for the reduction of the
/// expression e1 in function position in applications (e1 e2).
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct HybridApplicativeOrder<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for HybridApplicativeOrder<A>
where
    A: AlphaRename,
{
    /// Performs β-reduction on a given lambda expression applying a
    /// applicative-order strategy.
    fn reduce(mut expr: Term) -> Term {
        HybridApplicativeOrder::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> HybridApplicativeOrder<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
            Lam(_, ref mut body) => {
                HybridApplicativeOrder::<A>::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                CallByValue::<A>::reduce_rec(lhs);
                HybridApplicativeOrder::<A>::reduce_rec(rhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        HybridApplicativeOrder::<A>::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => {
                        HybridApplicativeOrder::<A>::reduce_rec(lhs);
                        None
                    },
                }
            },
            _ => None,
        } {
            *expr = subst;
        }
    }
}

/// Head-Spine [β-reduction] to head normal form.
///
/// * Reduces the leftmost outermost redex first.
/// * Performs reductions inside lambda abstractions, but only in head position.
///
/// This strategy is uniform as its definition involves no other reduction
/// strategy.
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct HeadSpine<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for HeadSpine<A>
where
    A: AlphaRename,
{
    /// Performs β-reduction on a given lambda expression applying a
    /// applicative-order strategy.
    fn reduce(mut expr: Term) -> Term {
        HeadSpine::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> HeadSpine<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
            Lam(_, ref mut body) => {
                HeadSpine::<A>::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref rhs) => {
                HeadSpine::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        HeadSpine::<A>::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst;
        }
    }
}

/// Hybrid-Normal-Order [β-reduction] to normal form.
///
/// * A hybrid of head-spine and normal-order reduction.
/// * Reduces the leftmost outermost redex first.
/// * In an application (e1 e2) the function term e1 is reduced using the
///   head-spine strategy ([`HeadSpine`](struct.HeadSpine.html)).
/// * Any redex that is contracted is the leftmost one not contained in any
///   other redex.
/// * Reductions are performed also under lambda abstractions.
///
/// This strategy is a hybrid as it uses head-spine for the reduction of the
/// expression e1 in function position in applications (e1 e2).
///
/// [β-reduction]: https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct HybridNormalOrder<A> {
    _alpha_rename: PhantomData<A>,
}

impl<A> BetaReduce for HybridNormalOrder<A>
where
    A: AlphaRename,
{
    /// Performs β-reduction on a given lambda expression applying a
    /// applicative-order strategy.
    fn reduce(mut expr: Term) -> Term {
        HybridNormalOrder::<A>::reduce_rec(&mut expr);
        expr
    }
}

impl<A> HybridNormalOrder<A>
where
    A: AlphaRename,
{
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst) = match *expr {
            Lam(_, ref mut body) => {
                HybridNormalOrder::<A>::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                HeadSpine::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        HybridNormalOrder::<A>::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => {
                        HybridNormalOrder::<A>::reduce_rec(lhs);
                        HybridNormalOrder::<A>::reduce_rec(rhs);
                        None
                    },
                }
            },
            _ => None,
        } {
            *expr = subst;
        }
    }
}
