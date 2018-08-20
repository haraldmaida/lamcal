
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
[lambal]: ..
[lamcal-repl]: .

[lamcal-repl] is a [Lambda Calculus] REPL command line application written in [Rust]. It can be used
to play around with lambda calculus expressions interactively.

This application is inspired by this [talk](https://www.youtube.com/watch?v=3VQ382QG-y4) of [glebec]
where he plays around with lambda calculus in a JavaScript console.   

[lamcal-repl] uses the [lamcal] library crate for the lambda calculus functionality and just adds
the REPL things to make it an application.

## Installation

To install the [lamcal-repl] command line application run the following commands in your terminal:

```
> cargo install lamcal-repl
```

After it is installed successfully [lamcal-repl] can be run by typing the name of the executable: 

```
> lamcali
```

Alternatively clone this git repository, go to the `repl` subdirectory and type `cargo run`, like so

```
> cd lamcal/repl
lamcal/repl> cargo run
```

## Usage

When the application starts it shows you should see the following in the command line:

```
info: Welcome to lamcal-repl, the Lambda Calculus Repl, version 0.1.0
λ>
```

Notice the prompt changes to `λ>`. To evaluate a lambda expression simple type it in, like so: 

```
λ> (\x.(\y.x y) a) b
```

After pressing enter, the expression gets parsed and the default α-conversion and β-reduction is
applied. The result is printed to the console, like so:

```
λ> (\x.(\y.x y) a) b
b a
```

We can use \ or λ in lambda expressions as the parser understands both backslash '\' as well as the
lambda symbol 'λ' as the start of an abstraction.

The repl support commands to control the behavior of the application. Commands always start with a
colon. Most important commands are:

* `:h` or `:help` to print out help information
* `:q` or `:quit` to quit the application

A list of all implemented commands is given in the help information (command `:h`).  

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
