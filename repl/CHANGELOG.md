# Change Log

All user visible changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/), as described
for Rust libraries in [RFC #1105](https://github.com/rust-lang/rfcs/blob/master/text/1105-api-evolution.md)

## 0.4.0 : 2018-10-06 : No stack overflow and inspected

* Upgrade to lamcal version 0.4
* Add inspected mode (command `:i`)

## 0.3.0 : 2018-09-08 : Beautiful environment

* Upgrade to lamcal version 0.3
* Rename commands for setting and printing alpha- und beta-strategy
* Add support for evaluation of lambda terms in environment
* Add command to expand terms only (no beta-reduction)
* Add command to apply beta-reduction (no expansion of predefined terms)
* Add `:let` command to add new bindings to the environment or replace existing ones
* Add commands for maintaining bindings in the environment (`:ls-env`, `clr-env`, `ld-env`)
* Improve help text
* Upgrade `rustyline` to version 2.0

## 0.2.1 : 2018-08-26 : Use different reduction strategies

* Set and print beta-reduction strategy to be used
* Set and print alpha-conversion strategy to be used

## 0.2.0 : 2018-08-26 : More reduction strategies

* bump version of `lamcal` crate to `0.2`

## 0.1.1 : 2018-08-25 : Documentation

* Add homepage link to Cargo.toml  

## 0.1.0 : 2018-08-25 : Newborn

* First release
