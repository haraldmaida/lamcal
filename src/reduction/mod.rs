//! Reduction system of the lambda calculus.
//!
//! [evaluation strategies]: https://en.wikipedia.org/wiki/Evaluation_strategy
//! [reduction strategies]: https://en.wikipedia.org/wiki/Lambda_calculus#Reduction_strategies

use std::cell::RefCell;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::mem;
use std::rc::Rc;

use environment::Environment;
use inspect::{Inspect, Limit, NoOp, Stop};
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
        let mut to_check = Vec::with_capacity(2);
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
        false
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
        alpha_tramp::<A>(self, names)
    }

    /// [Substitutes] all occurrences of `var` as free variables with the
    /// expression `rhs` recursively on the structure of this `Term`.
    ///
    /// [substitutes]: https://en.wikipedia.org/wiki/Lambda_calculus#Substitution
    pub fn substitute(&mut self, var: &VarName, rhs: &Term) {
        substitute_tramp(self, var, rhs)
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
    /// If the reduction of the term diverges it can go through an infinite
    /// sequence of evaluation steps. To avoid endless loops, a default limit
    /// of `u32::MAX` reduction steps is applied. Thus this method returns
    /// when either no more reduction is possible or the limit of `u32::MAX`
    /// iterations has been reached.
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

    /// Performs a [β-reduction] on this `Term` with inspection.
    ///
    /// The given inspection is called before each contraction (reduction step).
    /// See the documentation of the [`inspect`](mod.inspect.html) module for
    /// information on how to define an inspection and the implementations that
    /// are provided.
    ///
    /// The reduction strategy to be used must be given as the type parameter
    /// `B`.
    pub fn reduce_inspected<B, I>(&mut self, inspect: &mut I)
    where
        B: BetaReduce,
        I: Inspect,
    {
        let expr = mem::replace(self, dummy_term());
        *self = <B as BetaReduce>::reduce_inspected(expr, inspect)
    }

    /// Replaces free variables with the term bound to the variable's name in
    /// the given environment.
    ///
    /// This method walks through the whole term and replaces any free variable
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
        expand_tramp_inspected(self, env, &mut NoOp)
    }

    /// Replaces free variables with the term bound to the variable's name in
    /// the given environment with inspection.
    ///
    /// This method walks through the whole term and replaces any free variable
    /// with the term bound to the variable's name in the given environment.
    /// Bound variables are not replaced.
    ///
    /// Before each substitution of a variable with its bound term from the
    /// environment the given inspection is called. See the documentation
    /// of the [`inspect`](mod.inspect.html) module for information on how to
    /// define an inspection and the implementations that are provided.
    ///
    /// This method modifies this `Term` in place. If you want to expand named
    /// constants and get the result as a new `Term` while keeping the original
    /// `Term` unchanged use the standalone function
    /// [`expand`](fn.expand_inspected.html) instead.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate lamcal;
    /// # use lamcal::{app, expand_inspected, lam, var, Environment};
    /// # use lamcal::inspect::Collect;
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
    /// let mut collected = Collect::new();
    /// expr.expand_inspected(&env, &mut collected);
    ///
    /// assert_eq!(
    ///     collected.terms(),
    ///     &vec![
    ///         app![
    ///             var("C"),
    ///             lam("a", app(var("K"), var("I"))),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![
    ///             lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
    ///             lam("a", app(var("K"), var("I"))),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![
    ///             lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
    ///             lam("a", app(lam("a", lam("b", var("a"))), var("I"))),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///     ][..],
    /// );
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
    pub fn expand_inspected(&mut self, env: &Environment, inspect: &mut impl Inspect) {
        expand_tramp_inspected(self, env, inspect)
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
        expand_tramp_inspected(self, env, &mut NoOp);
        let expr = mem::replace(self, dummy_term());
        *self = <B as BetaReduce>::reduce(expr);
    }

    /// Evaluates this lambda expression with inspection in the given
    /// environment.
    ///
    /// For the β-reduction step a reduction strategy is required. Therefore the
    /// reduction strategy must be specified as the type parameter `B`.
    ///
    /// The given inspection is called before each substitution of a free
    /// variable with its bound term and before each contraction (reduction
    /// step). See the documentation of the [`inspect`](mod.inspect.html)
    /// for information on how to define an inspection and the implementations
    /// that are provided.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use]
    /// # extern crate lamcal;
    /// # use lamcal::{app, evaluate, lam, var, Enumerate, Environment, NormalOrder};
    /// # use lamcal::inspect::{Collect};
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
    /// let mut collected = Collect::new();
    ///
    /// expr.evaluate_inspected::<NormalOrder<Enumerate>, _>(&env, &mut collected);
    ///
    /// assert_eq!(
    ///     collected.terms(),
    ///     &vec![
    ///         app![
    ///             var("C"),
    ///             lam("x", lam("y", app(var("x"), var("y")))),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![
    ///             lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
    ///             lam("x", lam("y", app(var("x"), var("y")))),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![
    ///             lam(
    ///                 "b",
    ///                 lam(
    ///                     "c",
    ///                     app![
    ///                         lam("x", lam("y", app(var("x"), var("y")))),
    ///                         var("c"),
    ///                         var("b")
    ///                     ]
    ///                 )
    ///             ),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![
    ///             lam(
    ///                 "b",
    ///                 lam("c", app![lam("y", app(var("c"), var("y"))), var("b")])
    ///             ),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![
    ///             lam("b", lam("c", app(var("c"), var("b")),)),
    ///             var("e"),
    ///             var("f")
    ///         ],
    ///         app![lam("c", app(var("c"), var("e"))), var("f")],
    ///     ][..],
    /// );
    /// assert_eq!(expr, app(var("f"), var("e")));
    /// # }
    /// ```
    pub fn evaluate_inspected<B, I>(&mut self, env: &Environment, inspect: &mut I)
    where
        B: BetaReduce,
        I: Inspect,
    {
        expand_tramp_inspected(self, env, inspect);
        let expr = mem::replace(self, dummy_term());
        *self = <B as BetaReduce>::reduce_inspected(expr, inspect);
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
    expand_tramp_inspected(&mut expr2, env, &mut NoOp);
    <B as BetaReduce>::reduce(expr2)
}

/// Evaluates a lambda expression with inspection in the given environment.
///
/// This function takes the given expression by reference and returns a new
/// `Term` with the evaluation applied. The given `Term` remains unchanged.
///
/// For the β-reduction step a reduction strategy is required. Therefore the
/// reduction strategy must be specified as the type parameter `B`.
///
/// The given inspection is called before each substitution of a free variable
/// with its bound term from the environment and before each contraction
/// (reduction step). See the documentation of the
/// [`inspect`](mod.inspect.html) for information on how to define an
/// inspection and the implementations that are provided.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate lamcal;
/// # use lamcal::{app, evaluate_inspected, lam, var, Enumerate, Environment, NormalOrder};
/// # use lamcal::inspect::{Collect};
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
/// let mut collected = Collect::new();
///
/// let result = evaluate_inspected::<NormalOrder<Enumerate>, _>(&expr, &env, &mut collected);
///
/// assert_eq!(
///     collected.terms(),
///     &vec![
///         app![
///             var("C"),
///             lam("x", lam("y", app(var("x"), var("y")))),
///             var("e"),
///             var("f")
///         ],
///         app![
///             lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
///             lam("x", lam("y", app(var("x"), var("y")))),
///             var("e"),
///             var("f")
///         ],
///         app![
///             lam(
///                 "b",
///                 lam(
///                     "c",
///                     app![
///                         lam("x", lam("y", app(var("x"), var("y")))),
///                         var("c"),
///                         var("b")
///                     ]
///                 )
///             ),
///             var("e"),
///             var("f")
///         ],
///         app![
///             lam(
///                 "b",
///                 lam("c", app![lam("y", app(var("c"), var("y"))), var("b")])
///             ),
///             var("e"),
///             var("f")
///         ],
///         app![
///             lam("b", lam("c", app(var("c"), var("b")),)),
///             var("e"),
///             var("f")
///         ],
///         app![lam("c", app(var("c"), var("e"))), var("f")],
///     ][..],
/// );
/// assert_eq!(result, app(var("f"), var("e")));
/// # }
/// ```
pub fn evaluate_inspected<B, I>(expr: &Term, env: &Environment, inspect: &mut I) -> Term
where
    B: BetaReduce,
    I: Inspect,
{
    let mut expr2 = expr.clone();
    expand_tramp_inspected(&mut expr2, env, inspect);
    <B as BetaReduce>::reduce_inspected(expr2, inspect)
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
    expand_tramp_inspected(&mut expr2, env, &mut NoOp);
    expr2
}

/// Replaces free variables in a term with the term that is bound to the
/// variable's name in the given environment.
///
/// This function walks through the whole term and replaces any free variable
/// with the term bound to the variable's name in the given environment. Bound
/// variables are not replaced.
///
/// Before each substitution of a variable with its bound term from the
/// environment the given inspection is called. See the documentation
/// of the [`inspect`](mod.inspect.html) module for information on how to
/// define an inspection and the implementations that are provided.
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
/// # use lamcal::{app, expand_inspected, lam, var, Environment};
/// # use lamcal::inspect::Collect;
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
/// let mut collected = Collect::new();
/// let result = expand_inspected(&expr, &env, &mut collected);
///
/// assert_eq!(
///     collected.terms(),
///     &vec![
///         app![
///             var("C"),
///             lam("a", app(var("K"), var("I"))),
///             var("e"),
///             var("f")
///         ],
///         app![
///             lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
///             lam("a", app(var("K"), var("I"))),
///             var("e"),
///             var("f")
///         ],
///         app![
///             lam("a", lam("b", lam("c", app![var("a"), var("c"), var("b")]))),
///             lam("a", app(lam("a", lam("b", var("a"))), var("I"))),
///             var("e"),
///             var("f")
///         ],
///     ][..],
/// );
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
pub fn expand_inspected(expr: &Term, env: &Environment, inspect: &mut impl Inspect) -> Term {
    let mut expr2 = expr.clone();
    expand_tramp_inspected(&mut expr2, env, inspect);
    expr2
}

fn expand_tramp_inspected(expr: &mut Term, env: &Environment, inspect: &mut impl Inspect) {
    let mut todo: Vec<(RefCell<*mut Term>, HashSet<VarName>)> = Vec::with_capacity(2);
    todo.push((RefCell::new(expr), HashSet::with_capacity(4)));
    while let Some((term, mut bound_vars)) = todo.pop() {
        unsafe {
            let maybe_expand_with = match **term.borrow() {
                Var(ref name) if !bound_vars.contains(name) => env.lookup_term(name).cloned(),
                _ => None,
            };
            if let Some(mut expand_with) = maybe_expand_with {
                if Stop::Yes == inspect.inspect(expr) {
                    break;
                }
                **term.borrow_mut() = expand_with;
                todo.push((term, bound_vars));
            } else {
                match **term.borrow() {
                    Var(_) => {},
                    Lam(ref param, ref mut body) => {
                        bound_vars.insert(param.to_owned());
                        todo.push((RefCell::new(&mut **body), bound_vars));
                    },
                    App(ref mut lhs, ref mut rhs) => {
                        todo.push((RefCell::new(&mut **rhs), bound_vars.clone()));
                        todo.push((RefCell::new(&mut **lhs), bound_vars));
                    },
                }
            }
        }
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
    let mut expr2 = expr.clone();
    alpha_tramp::<A>(&mut expr2, names);
    expr2
}

fn alpha_tramp<A>(expr: &mut Term, names: &HashSet<&VarName>)
where
    A: AlphaRename,
{
    let free_vars: HashSet<VarName> = HashSet::from_iter(
        names
            .into_iter()
            .cloned()
            .cloned()
            .chain(expr.free_vars().into_iter().cloned()),
    );
    let mut todo = Vec::with_capacity(2);
    todo.push((expr, HashSet::<VarName>::with_capacity(4)));
    while let Some((term, mut bound_vars)) = todo.pop() {
        match *term {
            Var(ref mut name) => {
                if bound_vars.contains(name) {
                    while free_vars.contains(name) {
                        <A as AlphaRename>::rename(&mut **name);
                    }
                }
            },
            Lam(ref mut name, ref mut body) => {
                bound_vars.insert(name.to_owned());
                while free_vars.contains(name) {
                    <A as AlphaRename>::rename(&mut **name);
                }
                todo.push((body, bound_vars));
            },
            App(ref mut lhs, ref mut rhs) => {
                todo.push((rhs, bound_vars.clone()));
                todo.push((lhs, bound_vars));
            },
        }
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
    let mut expr2 = expr.clone();
    alpha_tramp::<A>(&mut expr2, &subst.free_vars());
    substitute_tramp(&mut expr2, var, subst);
    expr2
}

fn substitute_tramp(expr: &mut Term, var: &VarName, subst: &Term) {
    let mut todo: Vec<&mut Term> = Vec::with_capacity(2);
    todo.push(expr);
    while let Some(term) = todo.pop() {
        let do_subst = match term {
            Var(ref name) => name == var,
            _ => false,
        };
        if do_subst {
            *term = subst.clone();
        } else {
            match term {
                Var(_) => {},
                Lam(ref mut param, ref mut body) => {
                    if param != var {
                        todo.push(body);
                    }
                },
                App(ref mut lhs, ref mut rhs) => {
                    todo.push(rhs);
                    todo.push(lhs);
                },
            }
        }
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
    let expr2 = expr.clone();
    if let Lam(param, mut body) = expr2 {
        alpha_tramp::<A>(&mut body, &subst.free_vars());
        substitute_tramp(&mut body, &param, subst);
        *body
    } else {
        expr2
    }
}

fn apply_mut<A>(expr: &mut Term, subst: &Term)
where
    A: AlphaRename,
{
    if let Some(replace_with) = match *expr {
        Lam(ref param, ref mut body) => {
            alpha_tramp::<A>(body, &subst.free_vars());
            substitute_tramp(body, param, subst);
            Some(mem::replace(&mut **body, dummy_term()))
        },
        _ => None,
    } {
        *expr = replace_with;
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

/// Performs a [β-reduction] on a given lambda expression with inspection
/// applying the given reduction strategy and inspection.
///
/// The reduction strategy to be used must be given as the type parameter `B`.
///
/// The given inspection is called before each contraction (reduction step).
/// See the documentation of the [`inspect`](mod.inspect.html) for how to
/// define an inspection and the provided implementations.
///
/// This function returns the result as a new `Term`. The given `Term` remains
/// unchanged. If you want to apply a β-reduction modifying the term in place
/// use the associated function
/// [`Term::reduce_inspected`](enum.Term.html#method.reduce_inspected) instead.
pub fn reduce_inspected<B, I>(expr: &Term, inspect: &mut I) -> Term
where
    B: BetaReduce,
    I: Inspect,
{
    <B as BetaReduce>::reduce_inspected(expr.clone(), inspect)
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
    ///
    /// The default implementation limits the reduction to `Limit::default()`
    /// reduction steps to prevent endless loops on diverging expressions.
    fn reduce(expr: Term) -> Term {
        Self::reduce_inspected(expr, &mut Limit::default())
    }

    /// Performs one step of β-reduction on the given `Term` and returns the
    /// result.
    fn reduce_once(expr: Term) -> Term {
        Self::reduce_inspected(expr, &mut Limit::new(1))
    }

    /// Performs β-reduction allowing to inspect the current term before
    /// each contraction.
    ///
    /// Implementations must call the `Inspect::inspect` function of the given
    /// `Inspect` instance exactly once before each contraction and respect
    /// their return value. If the `Inspect::inspect` function returns
    /// `Stop::Yes` the reduction must be stopped immediately, so that no
    /// further reduction is performed.
    fn reduce_inspected(expr: Term, inspect: &mut impl Inspect) -> Term;
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> CallByName<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            App(ref mut lhs, ref rhs) => {
                Self::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_left(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref rhs) => match **lhs {
                        Lam(_, _) => {
                            apply_mut::<A>(lhs, rhs);
                            // defer actual substitution outside match expression
                            // because of the borrow checker
                            //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                            mem::swap(&mut temp_term, &mut **lhs);
                            true
                        },
                        _ => false,
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_left(term, &mut parents);
                }
            }
        }
    }
}

unsafe fn descend_left(term: Rc<RefCell<*mut Term>>, parents: &mut Vec<Rc<RefCell<*mut Term>>>) {
    let mut to_check: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(1);
    to_check.push(term);
    while let Some(term) = to_check.pop() {
        if let App(ref mut lhs, _) = **term.borrow_mut() {
            parents.push(term.clone());
            if let App(_, _) = **lhs {
                to_check.push(Rc::new(RefCell::new(&mut **lhs)));
            } else {
                break;
            }
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> NormalOrder<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            Lam(_, ref mut body) => {
                Self::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                CallByName::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => {
                        Self::reduce_rec(lhs);
                        Self::reduce_rec(rhs);
                        None
                    },
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_left_and_body(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref mut rhs) => {
                        CallByName::<A>::reduce_inspected_mut(lhs, inspect);
                        match **lhs {
                            Lam(_, _) => {
                                apply_mut::<A>(lhs, rhs);
                                // defer actual substitution outside match expression
                                // because of the borrow checker
                                //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                                mem::swap(&mut temp_term, &mut **lhs);
                                true
                            },
                            _ => {
                                descend_left_and_right_and_body(
                                    Rc::new(RefCell::new(&mut **rhs)),
                                    &mut parents,
                                );
                                descend_left_and_right_and_body(
                                    Rc::new(RefCell::new(&mut **lhs)),
                                    &mut parents,
                                );
                                false
                            },
                        }
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_left_and_right_and_body(term, &mut parents);
                }
            }
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> CallByValue<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            App(ref mut lhs, ref mut rhs) => {
                Self::reduce_rec(lhs);
                Self::reduce_rec(rhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_left_and_right(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref mut rhs) => match **lhs {
                        Lam(_, _) => {
                            apply_mut::<A>(lhs, rhs);
                            // defer actual substitution outside match expression
                            // because of the borrow checker
                            //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                            mem::swap(&mut temp_term, &mut **lhs);
                            true
                        },
                        _ => false,
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_left_and_right(term.clone(), &mut parents);
                }
            }
        }
    }
}

unsafe fn descend_left_and_right(
    term: Rc<RefCell<*mut Term>>,
    parents: &mut Vec<Rc<RefCell<*mut Term>>>,
) {
    let mut to_check: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(2);
    to_check.push(term);
    while let Some(term) = to_check.pop() {
        if let App(ref mut lhs, ref mut rhs) = **term.borrow_mut() {
            parents.push(term.clone());
            if let App(_, _) = **rhs {
                to_check.push(Rc::new(RefCell::new(&mut **rhs)));
            }
            if let App(_, _) = **lhs {
                parents.push(term.clone());
                to_check.push(Rc::new(RefCell::new(&mut **lhs)));
            }
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> ApplicativeOrder<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            Lam(_, ref mut body) => {
                Self::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                Self::reduce_rec(lhs);
                Self::reduce_rec(rhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_left_and_right_and_body(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref rhs) => match **lhs {
                        Lam(_, _) => {
                            apply_mut::<A>(lhs, rhs);
                            // defer actual substitution outside match expression
                            // because of the borrow checker
                            //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                            mem::swap(&mut temp_term, &mut **lhs);
                            true
                        },
                        _ => false,
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_left_and_right_and_body(term, &mut parents);
                }
            }
        }
    }
}

unsafe fn descend_left_and_right_and_body(
    term: Rc<RefCell<*mut Term>>,
    parents: &mut Vec<Rc<RefCell<*mut Term>>>,
) {
    let mut to_check: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(2);
    to_check.push(term);
    while let Some(term) = to_check.pop() {
        match **term.borrow_mut() {
            Lam(_, ref mut body) => {
                to_check.push(Rc::new(RefCell::new(&mut **body)));
            },
            App(ref mut lhs, ref mut rhs) => {
                parents.push(term.clone());
                match **rhs {
                    App(_, _) => {
                        to_check.push(Rc::new(RefCell::new(&mut **rhs)));
                    },
                    Lam(_, _) => {
                        to_check.push(Rc::new(RefCell::new(&mut **rhs)));
                    },
                    _ => {},
                }
                match **lhs {
                    App(_, _) => {
                        parents.push(term.clone());
                        to_check.push(Rc::new(RefCell::new(&mut **lhs)));
                    },
                    Lam(_, _) => {
                        to_check.push(Rc::new(RefCell::new(&mut **lhs)));
                    },
                    _ => {},
                }
            },
            _ => {},
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> HybridApplicativeOrder<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            Lam(_, ref mut body) => {
                Self::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                CallByValue::<A>::reduce_rec(lhs);
                Self::reduce_rec(rhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => {
                        Self::reduce_rec(lhs);
                        None
                    },
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_right_and_body(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref rhs) => {
                        CallByValue::<A>::reduce_inspected_mut(lhs, inspect);
                        match **lhs {
                            Lam(_, _) => {
                                apply_mut::<A>(lhs, rhs);
                                // defer actual substitution outside match expression
                                // because of the borrow checker
                                //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                                mem::swap(&mut temp_term, &mut **lhs);
                                true
                            },
                            _ => false,
                        }
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_right_and_body(term, &mut parents);
                }
            }
        }
    }
}

unsafe fn descend_right_and_body(
    term: Rc<RefCell<*mut Term>>,
    parents: &mut Vec<Rc<RefCell<*mut Term>>>,
) {
    let mut to_check: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(1);
    to_check.push(term);
    while let Some(term) = to_check.pop() {
        match **term.borrow_mut() {
            Lam(_, ref mut body) => {
                to_check.push(Rc::new(RefCell::new(&mut **body)));
            },
            App(_, ref mut rhs) => {
                parents.push(term.clone());
                match **rhs {
                    App(_, _) => {
                        to_check.push(Rc::new(RefCell::new(&mut **rhs)));
                    },
                    Lam(_, _) => {
                        to_check.push(Rc::new(RefCell::new(&mut **rhs)));
                    },
                    _ => break,
                }
            },
            _ => break,
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> HeadSpine<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            Lam(_, ref mut body) => {
                Self::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref rhs) => {
                Self::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => None,
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_left_and_body(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref rhs) => match **lhs {
                        Lam(_, _) => {
                            apply_mut::<A>(lhs, rhs);
                            // defer actual substitution outside match expression
                            // because of the borrow checker
                            //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                            mem::swap(&mut temp_term, &mut **lhs);
                            true
                        },
                        _ => false,
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_left_and_body(term, &mut parents);
                }
            }
        }
    }
}

unsafe fn descend_left_and_body(
    term: Rc<RefCell<*mut Term>>,
    parents: &mut Vec<Rc<RefCell<*mut Term>>>,
) {
    let mut to_check: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(1);
    to_check.push(term);
    while let Some(term) = to_check.pop() {
        match **term.borrow_mut() {
            Lam(_, ref mut body) => {
                to_check.push(Rc::new(RefCell::new(&mut **body)));
            },
            App(ref mut lhs, _) => {
                parents.push(term.clone());
                to_check.push(Rc::new(RefCell::new(&mut **lhs)));
            },
            _ => break,
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
    fn reduce_inspected(mut expr: Term, inspect: &mut impl Inspect) -> Term {
        Self::reduce_inspected_mut(&mut expr, inspect);
        expr
    }
}

impl<A> HybridNormalOrder<A>
where
    A: AlphaRename,
{
    #[cfg(test)]
    fn reduce_rec(expr: &mut Term) {
        if let Some(subst_with) = match *expr {
            Lam(_, ref mut body) => {
                Self::reduce_rec(body);
                None
            },
            App(ref mut lhs, ref mut rhs) => {
                HeadSpine::<A>::reduce_rec(lhs);
                match **lhs {
                    Lam(_, _) => {
                        apply_mut::<A>(lhs, rhs);
                        Self::reduce_rec(lhs);
                        // defer actual substitution outside match expression
                        // because of the borrow checker
                        //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                        Some(mem::replace(&mut **lhs, dummy_term()))
                    },
                    _ => {
                        Self::reduce_rec(lhs);
                        Self::reduce_rec(rhs);
                        None
                    },
                }
            },
            _ => None,
        } {
            *expr = subst_with;
        }
    }

    fn reduce_inspected_mut(expr: &mut Term, inspect: &mut impl Inspect) {
        let mut temp_term = dummy_term();
        let mut parents: Vec<Rc<RefCell<*mut Term>>> = Vec::with_capacity(8);
        let base_term: Rc<RefCell<*mut Term>> = Rc::new(RefCell::new(expr));
        unsafe {
            descend_left_and_body(base_term.clone(), &mut parents);
            while let Some(term) = parents.pop() {
                if Stop::Yes == match **term.borrow() {
                    App(ref lhs, _) => match **lhs {
                        Lam(_, _) => inspect.inspect(&**base_term.borrow()),
                        _ => Stop::No,
                    },
                    _ => Stop::No,
                } {
                    break;
                }
                let do_swap = match **term.borrow_mut() {
                    App(ref mut lhs, ref mut rhs) => {
                        HeadSpine::<A>::reduce_inspected_mut(lhs, inspect);
                        match **lhs {
                            Lam(_, _) => {
                                apply_mut::<A>(lhs, rhs);
                                // defer actual substitution outside match expression
                                // because of the borrow checker
                                //TODO refactor when non-lexical-lifetimes are stabilized, see [issue 43234](https://github.com/rust-lang/rust/issues/43234)
                                mem::swap(&mut temp_term, &mut **lhs);
                                true
                            },
                            _ => {
                                descend_left_and_right_and_body(
                                    Rc::new(RefCell::new(&mut **rhs)),
                                    &mut parents,
                                );
                                descend_left_and_right_and_body(
                                    Rc::new(RefCell::new(&mut **lhs)),
                                    &mut parents,
                                );
                                false
                            },
                        }
                    },
                    _ => false,
                };
                if do_swap {
                    term.borrow_mut().swap(&mut temp_term);
                    parents.push(term.clone());
                    descend_left_and_right_and_body(term, &mut parents);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
