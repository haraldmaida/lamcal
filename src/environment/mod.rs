//! The environment is the context in which a lambda term is evaluated.

use std::collections::HashMap;
use std::fmt::{self, Display};
use std::iter::FromIterator;

use combinator;
use term::{ConstName, Term};

/// Constructs a new `Binding` with the given term bound to the given name.
pub fn bind(name: impl Into<String>, term: impl Into<Term>) -> Binding {
    Binding::new(ConstName(name.into()), term.into())
}

/// The environment in for evaluating lambda terms.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    bindings: HashMap<ConstName, Term>,
}

impl Environment {
    /// Creates a new empty environment.
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
        }
    }

    /// Adds the given binding to this environment.
    pub fn insert_binding(&mut self, binding: Binding) {
        let (name, term) = binding.unwrap();
        self.bindings.insert(name, term);
    }

    /// Adds a new binding of the given term to the given name to this
    /// environment.
    pub fn bind(&mut self, name: ConstName, term: Term) -> Option<Term> {
        self.bindings.insert(name, term)
    }

    /// Removes the binding to the given name from this environment and returns
    /// the bound term if such a binding previously existed.
    pub fn unbind(&mut self, name: &ConstName) -> Option<Term> {
        self.bindings.remove(name)
    }

    /// Looks up the binding for a name and returns a reference to the bound
    /// term if a binding exists for the given name.
    pub fn lookup_term(&self, name: &ConstName) -> Option<&Term> {
        self.bindings.get(name)
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::from_iter(combinator::complete_set())
    }
}

impl FromIterator<(ConstName, Term)> for Environment {
    fn from_iter<I>(set: I) -> Self
    where
        I: IntoIterator<Item = (ConstName, Term)>,
    {
        Environment {
            bindings: HashMap::from_iter(set.into_iter()),
        }
    }
}

impl FromIterator<Binding> for Environment {
    fn from_iter<I>(set: I) -> Self
    where
        I: IntoIterator<Item = Binding>,
    {
        Environment {
            bindings: HashMap::from_iter(set.into_iter().map(|b| (b.name, b.term))),
        }
    }
}

impl Extend<(ConstName, Term)> for Environment {
    fn extend<I: IntoIterator<Item = (ConstName, Term)>>(&mut self, iter: I) {
        self.bindings.extend(iter)
    }
}

impl Extend<Binding> for Environment {
    fn extend<I: IntoIterator<Item = Binding>>(&mut self, iter: I) {
        self.bindings
            .extend(iter.into_iter().map(|b| (b.name, b.term)))
    }
}

/// A binding definition.
///
/// It represents a lambda term bound to a name. Bindings are stored in an
/// environment. When evaluating lambda terms names bound terms are looked up
/// by name and replace the name with the bound term.
///
/// Bindings can be constructed programmatically using
/// [`Binding::new`](struct.Binding.html#method.new) or the more convenient
/// function [`bind`](fn.bind.html).
///
/// The parser of this crate also supports bindings. In the syntax of the
/// parser bindings are defined by `let` statements. For more details see the
/// [`parse`](fn.parse.html) function and the
/// [the parser chapter](index.html#the-parser) in the crate documentation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binding {
    name: ConstName,
    term: Term,
}

impl Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} = {}", self.name, self.term)
    }
}

impl Binding {
    /// Constructs a new `Binding` of given term to the given name.
    pub fn new(name: ConstName, term: Term) -> Self {
        Binding { name, term }
    }

    /// Deconstruct this binding into its name and the bound term and returns
    /// both as a tuple.
    pub fn unwrap(self) -> (ConstName, Term) {
        (self.name, self.term)
    }

    /// Returns a reference to the name of this binding.
    pub fn name(&self) -> &ConstName {
        &self.name
    }

    /// Returns a reference to the term that is bound by this binding's name.
    pub fn term(&self) -> &Term {
        &self.term
    }
}

/// Constructs a `Binding`s from an identifier to a `Term`.
///
/// The bind! macro can be used to conveniently construct a `Binding` either
/// from an identifier and a `Term` or a `String` and a `Term`.
///
/// # Example 1
///
/// This example binds the identifier of an combinator function to its term.
///
/// ```
/// #[macro_use]
/// extern crate lamcal;
/// use lamcal::combinator::I;
/// use lamcal::{bind, lam, var};
///
/// # fn main() {
/// let binding = bind!(I => I());
///
/// assert_eq!(binding, bind("I", lam("a", var("a"))));
/// # }
/// ```
///
/// # Example 2
///
/// This example binds a name to an arbitrary term.
///
/// ```
/// #[macro_use]
/// extern crate lamcal;
/// use lamcal::combinator::I;
/// use lamcal::{bind, lam, var};
///
/// # fn main() {
/// let binding = bind! {
///     "Hummingbird" => lam("a", lam("b", lam("c", app![var("a"), var("b"), var("c"), var("b")])))
/// };
///
/// assert_eq!(
///     binding,
///     bind(
///         "Hummingbird",
///         lam(
///             "a",
///             lam("b", lam("c", app![var("a"), var("b"), var("c"), var("b")]))
///         )
///     )
/// );
/// # }
/// ```
#[macro_export]
macro_rules! bind {
    ($name:ident => $term:expr) => {
        $crate::Binding::new($crate::ConstName(String::from(stringify!($name))), $term)
    };
    ($name:expr => $term:expr) => {
        $crate::Binding::new($crate::ConstName(String::from($name)), $term)
    };
}

/// Constructs a set of `Binding`s from identifier to `Term`.
///
/// The binds! macro can be used to conveniently construct multiple `Binding`s.
/// The resulting set of bindings is returned as a `std::collections::HashSet`.
///
/// # Example
///
/// This example creates a set of bindings.
///
/// ```
/// #[macro_use]
/// extern crate lamcal;
/// use lamcal::combinator::{I, K};
/// use lamcal::{bind, lam, var};
/// use std::collections::HashSet;
///
/// # fn main() {
/// let bindings = binds! {
///     "I" => I(),
///     "Hummingbird" => lam("a", lam("b", lam("c", app![var("a"), var("b"), var("c"), var("b")]))),
///     "K" => K(),
/// };
///
/// let mut expected = HashSet::new();
/// expected.insert(bind("I", lam("a", var("a"))));
/// expected.insert(bind(
///     "Hummingbird",
///     lam(
///         "a",
///         lam("b", lam("c", app![var("a"), var("b"), var("c"), var("b")])),
///     ),
/// ));
/// expected.insert(bind("K", lam("a", lam("b", var("a")))));
///
/// assert_eq!(bindings, expected);
/// # }
/// ```
#[macro_export]
macro_rules! binds {
    ($($name:ident => $term:expr),* $(,)*) => {
        {
            let mut binds = HashSet::new();
            $(
                binds.insert($crate::Binding::new($crate::ConstName(String::from(stringify!($name))), $term));
            )*
            binds
        }
    };
    ($($name:expr => $term:expr),* $(,)*) => {
        {
            let mut binds = HashSet::new();
            $(
                binds.insert($crate::Binding::new($crate::ConstName(String::from($name)), $term));
            )*
            binds
        }
    };
}

#[cfg(test)]
mod tests;
