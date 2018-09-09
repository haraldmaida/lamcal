
# Lamcal REPL

[![Crates.io][crates_badge]][crate]
[![Docs.rs][docs_badge]][documentation]
[![Linux Build Status][travis_badge]][Travis CI]
[![Windows Build Status][appveyor_badge]][Appveyor CI]
[![codevoc.io][codecov_badge]][codecoverage]
[![Apache-2.0][license_badge]][Apache-2.0]

[crates_badge]: https://img.shields.io/crates/v/lamcal-repl.svg
[docs_badge]: https://docs.rs/lamcal-repl/badge.svg
[travis_badge]: https://travis-ci.org/haraldmaida/lamcal.svg?branch=master
[appveyor_badge]: https://ci.appveyor.com/api/projects/status/github/haraldmaida/lamcal?branch=master&svg=true
[codecov_badge]: https://codecov.io/gh/haraldmaida/lamcal/branch/master/graph/badge.svg
[license_badge]: https://img.shields.io/badge/license-Apache%2D%2D2%2E0-blue.svg

[crate]: https://crates.io/crates/lamcal-repl
[documentation]: https://docs.rs/lamcal-repl
[Travis CI]: https://travis-ci.org/haraldmaida/lamcal
[Appveyor CI]: https://ci.appveyor.com/project/haraldmaida/lamcal
[codecoverage]: https://codecov.io/github/haraldmaida/lamcal?branch=master
[Apache-2.0]: https://www.apache.org/licenses/LICENSE-2.0
[license]: LICENSE
[lamcal]: https://github.com/haraldmaida/lamcal
[lamcal-repl]: .

[lamcal-repl] is a [Lambda Calculus] REPL command line application written in [Rust]. It can be used
to play around with lambda calculus expressions interactively.

This application is inspired by this [talk](https://www.youtube.com/watch?v=3VQ382QG-y4) by [glebec]
where he plays around with lambda calculus in a JavaScript console.   

[lamcal-repl] uses the [lamcal] library crate for the lambda calculus functionality and adds
the REPL things to make it an application. The name of the executable is `lamcali`.

## Installation

To install the [lamcal-repl] command line application run the following commands in your terminal:

```
> cargo install lamcal-repl
```

After it has been installed successfully we can run [lamcal-repl] by typing the name of the
executable: 

```
> lamcali
```

Alternatively we can clone this git repository, go to the `repl` subdirectory and type `cargo run`, 
like so

```
> cd lamcal/repl
lamcal/repl> cargo run
```

## Usage

When the application starts we see the following in the command line:

```
info: Welcome to lamcal-repl, the Lambda Calculus REPL, version 0.3.0
λ>
```

Notice that the command line prompt changes to `λ>`. To evaluate a lambda expression we simple type
in the expression at the prompt, like so: 

```
λ> (\x.(\y.x y) a) b
```

After pressing enter, the expression gets parsed and the default α-conversion and β-reduction is
applied. The result is printed to the console, like so:

```
λ> (\x.(\y.x y) a) b
b a
```

We can use the backslash character \ or the greek lowercase lambda λ to denote a lambda abstraction
with a lambda expressions. The parser understands both backslash as well as the lowercase lambda 
symbol as the start of an abstraction.

The repl support commands to control the behavior of the application. Commands always start with a
colon. Most important commands are:

* `:h` or `:help` to print out help information
* `:q` or `:quit` to quit the application

A list of all implemented commands is given in the help information (command `:h`).

### let bindings and the environment

When playing around with more complex expressions typing them can be tedious. Therefore the `lamcal`
crate provides the possibility to evaluate terms in an environment with predefined terms bound to
names. During evaluation free variables with a name that is bound to a term in the environment is
replaced by the bound term. On startup of the REPL it instantiates a default environment as the 
global environment. The global environment is used when evaluating expression by default.

To add new bindings to the environment or replace existing ones we use the `:let` command. For 
example the following command binds the name `rev` to the term `λf.λa.λb.f b a`

```
λ> :let rev = λf.λa.λb.f b a
```

Now when we use `rev` as a free variable in any expression it will be replaced by the whole 
expression to the right side of the equal sign.

```
λ> rev
λf.λa.λb.f b a

λ> rev f x y
f y x
```

The last evaluation is equal to typing:

```
λ> (λf.λa.λb.f b a) f x y
f y x
```

If we want to perform a beta-reduction without expanding bound names we can use the `:b` command
like so:

```
λ> :b (λa.rev a) f x y
rev f x y
```

To expand bound names without reducing the term we can use the `:x` command:

```
λ> :x (λa.rev a) f x y
(λa.(λf.λa.λb.f b a) a) f x y
``` 

### Commands

The following commands are available:

    :h or :help       displays this help information

    :q or :quit       quits the repl session

    :v or :version    prints out the version of lamcali

    :e <expr> or :eval <expr>
                      evaluates the lambda expression <expr>. This command is
                      equivalent to just typing a lambda expression and
                      pressing [enter].

    :b <expr> or :beta <expr>
                      performs a beta-reduction on the lambda expression <expr>
                      using the current set strategy.

    :x <expr> or :expand <expr>
                      replaces free variables in the lambda expression <expr>
                      with the expression bound to the variable's name in the
                      current environment.

    :p <expr> or :parse <expr>
                      parses the lambda expression <expr> and prints out the
                      abstract syntax tree (AST) of the lambda expression.

    :let <name> = <expr>
                      defines a new binding of <expr> to <name> and adds it to
                      the environment. If a binding with the same name
                      previously existed in the environment it is replaced by
                      the new binding.

    :bs or :beta-strategy
                      prints the current set beta-reduction strategy.

    :bs <strategy> or :beta_strategy <strategy>
                      set the beta-reduction strategy to <strategy>.
                      <strategy> can be one of:
                      app : applicative-order reducing to normal form
                      cbn : call-by-name reducing to weak head normal form
                      cbv : call-by-value reducing to weak normal form
                      hap : hybrid-applicative-order reducing to normal form
                      hno : hybrid-normal-order reducing to normal form
                      hsp : head-spine reducing to head normal form
                      nor : normal-order reducing to normal form (the default)

    :as or :alpha-strategy
                      prints the current set alpha-conversion strategy.

    :as <strategy> or :alpha-strategy <strategy>
                      set the alpha-conversion strategy to <strategy>.
                      <strategy> can be one of:
                      enumerate : appending increasing digits (the default)
                      prime     : appending tick symbols

    :clr-env          clears the environment, all bindings are removed

    :ld-env default   loads the default set of predefined bindings into the
                      environment. Existing bindings with the same name as a
                      binding in the default set will be replaced. Existing
                      bindings with no name clash will not be changed.

    :ls-env           lists all bindings defined in the environment

    :ls-env <pattern> lists all bindings filtered by <pattern>. It lists all
                      bindings with a name that contains the given pattern as a
                      substring (ignoring case).
  

## License

Licensed under Apache License, Version 2.0<br/>
see [LICENSE] or http://www.apache.org/licenses/LICENSE-2.0 for details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.

--------------------------------------------------------------------------------
[de bruijn index]: https://en.wikipedia.org/wiki/De_Bruijn_index
[krivine machine]: https://en.wikipedia.org/wiki/Krivine_machine
[lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[lcss]: https://www.youtube.com/watch?v=GYCYq0lEFhE
[rust]: https://www.rust-lang.org
[SECD machine]: https://en.wikipedia.org/wiki/SECD_machine
[glebec]: https://github.com/glebec
