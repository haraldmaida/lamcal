# Change Log

All user visible changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/), as described
for Rust libraries in [RFC #1105](https://github.com/rust-lang/rfcs/blob/master/text/1105-api-evolution.md)

## 0.3.0 : 2018-09-08 : Beautiful environment

### Breaking changes

* rename `Var` struct to `VarName`

### Features

* allow uppercase letters in variable names
* allow variable names starting with digits
* provide `Environment` to hold bindings of `Term`s to names
* add `bind!` and `binds!` macros for convenient definition of bindings
* add `expand` and `eval` functions to evaluate lambda terms in an environment
* add `Term::free_vars` method to determine all free variables in a term
* add predefined lambda combinators (see `combinator` module)
* add Church encoding for boolean and numerals (see `church_encoded` module)

### Bug Fixes

* wrong behavior of alpha-conversion
* fix bug in `Term::is_beta_redex` and `Term::is_beta_normal` functions
* required parenthesis omitted in display string of `Term`
* macro app! needs to many manually import required elements

## 0.2.0 : 2018-08-26 : More reduction strategies

### Bug Fixes

* export `Hint` struct 

### Features

* Add `HybridNormalOrder` beta-reduction strategy
* Add `HybridHeadSpine` beta-reduction strategy
* Add `HybridApplicativeOrder` beta-reduction strategy
* Add `ApplicativeOrder` beta-reduction strategy
* Add `CallByValue` beta-reduction strategy

### Documentation

* Improve documentation of reduction strategies

## 0.1.1 : 2018-08-25 : Documentation

* Add homepage link to Cargo.toml  

## 0.1.0 : 2018-08-25 : Newborn

* First release
