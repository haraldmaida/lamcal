//! [Church encoding] of data types, data structures and operations.
//!
//! This module defines Church encoded data types like boolean or numerals and
//! data structures like pair and list as well as operations with that data.
//!
//! The data types and data structures defined in this module are also
//! predefined named constants in the default environment which can be created
//! by calling `Environment::default()`.
//!
//! [church encoding]: https://en.wikipedia.org/wiki/Church_encoding

pub mod boolean;
pub mod numeral;

use std::collections::HashSet;

use environment::Binding;

pub use self::boolean::default_bindings as boolean_set;
pub use self::numeral::default_bindings as numeral_set;

/// Creates a set of bindings for all data types, data structures and operators
/// implemented in this module and its submodules.
pub fn default_bindings() -> HashSet<Binding> {
    let mut bindings = HashSet::new();
    bindings.extend(boolean_set());
    bindings.extend(numeral_set());
    bindings
}
