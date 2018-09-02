//! The environment is the context in which a lambda term is evaluated.

use std::collections::HashMap;
use std::fmt::{self, Display};

use term::{ConstName, Term};

/// Constructs a new `Binding` with the given term bound to the given name.
pub fn bind(name: impl Into<String>, term: impl Into<Term>) -> Binding {
    Binding::new(ConstName(name.into()), term.into())
}

/// The environment in for evaluating lambda terms.
#[derive(Default, Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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

#[cfg(test)]
mod tests;
