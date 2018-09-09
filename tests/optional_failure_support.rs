#![cfg(feature = "failure")]

extern crate failure;

extern crate lamcal;

use failure::Error;

use lamcal::{parse, Term};

fn custom_parse(input: &str) -> Result<Term, Error> {
    parse(input.chars()).map_err(From::from)
}

#[test]
fn convert_parse_error_into_failure_error() {
    let parsed = custom_parse("\\x.\\y y");

    assert_eq!(
        parsed.unwrap_err().to_string(),
"[AbstractionBodyExpected]: expected a '.' character as start of the body of the lambda abstraction
   1:7   \t   found:\tidentifier
        \texpected:\tthe '.' character as start of the lambda body
   hint: a lambda abstraction must contain a body, that is an expression following the '.' character
"
    );
}
