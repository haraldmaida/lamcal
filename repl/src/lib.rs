//! A REPL (read-evaluate-print loop) library for lambda calculus.
//!
//! Based on the [`lamcal`](https://docs.rs/crate/lamcal) crate.

#![doc(html_root_url = "https://docs.rs/lamcal-repl/0.2.1")]
#![warn(
    bare_trait_objects,
    missing_copy_implementations,
    missing_debug_implementations,
//    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unstable_features,
    unused_extern_crates,
    unused_import_braces,
    unused_qualifications,
)]

#[cfg(feature = "config")]
extern crate config;

extern crate lamcal;

pub mod command;
pub mod context;
pub mod model;
pub mod settings;
