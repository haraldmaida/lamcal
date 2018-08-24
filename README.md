
# Lamcal

[![Crates.io][crates_badge]][crate]
[![Docs.rs][docs_badge]][documentation]
[![Linux Build Status][travis_badge]][Travis CI]
[![Windows Build Status][appveyor_badge]][Appveyor CI]
[![codevoc.io][codecov_badge]][codecoverage]
[![Apache-2.0][license_badge]][Apache-2.0]

[crates_badge]: https://img.shields.io/crates/v/lamcal.svg
[docs_badge]: https://docs.rs/lamcal/badge.svg
[travis_badge]: https://travis-ci.org/haraldmaida/lamcal.svg?branch=master
[appveyor_badge]: https://ci.appveyor.com/api/projects/status/github/haraldmaida/lamcal?branch=master&svg=true
[codecov_badge]: https://codecov.io/gh/haraldmaida/lamcal/branch/master/graph/badge.svg
[license_badge]: https://img.shields.io/badge/license-Apache%2D%2D2%2E0-blue.svg

[crate]: https://crates.io/crates/lamcal
[documentation]: https://docs.rs/lamcal
[Travis CI]: https://travis-ci.org/haraldmaida/lamcal
[Appveyor CI]: https://ci.appveyor.com/project/innoave/lamcal
[codecoverage]: https://codecov.io/github/haraldmaida/lamcal?branch=master
[Apache-2.0]: https://www.apache.org/licenses/LICENSE-2.0
[license]: LICENSE
[lamcal]: https://github.com/haraldmaida/lamcal
[lamcal-repl]: repl

[lamcal] is a [Lambda Calculus] parser and evaluator written in [Rust]. It can be used to
    
* parse lambda expressions in classic notation, like `(λx.(λy.x y) a) b` or `(\x.(\y.x y) a) b`
  into terms
* construct terms programmatically using functions, e.g. `lam("x", app(var("x"), var("y")))`
* construct a sequence of function applications using the macro `app!`, e.g.
  `app![var("a"), var("b"), var("c")]` which is equivalent to 
  `app(app(var("a"), var("b")), var("c))` 
* apply α-conversion to terms using different strategies, such as enumeration or appending the tick
  symbol
* apply β-reduction to terms using different strategies, such as call-by-name, normal-order or 
  call-by-value
* be extended by implementing user specific strategies for α-conversion and β-reduction.

The separate crate [lamcal-repl] provides a command line REPL (read-evaluate-print-loop) application
to play around with lambda calculus terms and applying α-conversion and β-reduction interactively.

## Usage

To use [lamcal] as a library in your project add this to your `Cargo.toml` file:

```toml
[dependencies]
lamcal = "0.1"
```

and this to your crate root:

```rust
extern crate lamcal;
```

For details about the library see the [documentation] at crates.io.

This library optionally supports the `failure` crate. The support for the `failure` crate is a crate
feature. To enable it add the dependency to your `Cargo.toml` like so:

```toml
[dependencies]
lamcal = { version = "0.1", features = ["failure"] }
```

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
