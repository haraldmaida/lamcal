#![allow(missing_debug_implementations)]

#[cfg(test)]
mod tests;

use std::iter::IntoIterator;

use combine::char::{char, digit, lower, spaces};
use combine::combinator::{between, choice, many, many1, one_of, satisfy};
use combine::stream::state::{DefaultPositioned, Positioner, SourcePosition, State};
pub use combine::{ParseError, Parser, Stream, StreamOnce};

use term::{Term, Var};
//
//pub fn parse<I>(input: I) -> Result<Term, I::Error>
//where
//    I: Stream<Item = char> + StreamOnce<Item = char, Position =
// SourcePosition> + DefaultPositioned,    I::Positioner: Positioner<char>,
//    I::Error: ParseError<char, I::Range, SourcePosition>,
//{
//    expression().parse(State::new(input))
//}

parser!{
    pub fn expression[I]()(I) -> Term
    where [I: Stream<Item = char>]
    {
        expression_()
    }
}

fn expression_<I>() -> impl Parser<Input = I, Output = Term>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(spaces().with(choice((
        variable().map(From::from),
        abstraction().map(|(param, body)| Term::lam(param, body)),
        application().map(|(expr1, expr2)| Term::app(expr1, expr2)),
    )))).map(|seq: Vec<Term>| {
        seq.into_iter()
            .fold(None, |acc, expr2| match acc {
                Some(expr1) => Some(Term::app(expr1, expr2)),
                None => Some(expr2),
            }).unwrap()
    })
}

parser!{
    pub fn abstraction[I]()(I) -> (Var, Term)
    where [I: Stream<Item = char>]
    {
        abstraction_()
    }
}

fn abstraction_<I>() -> impl Parser<Input = I, Output = (Var, Term)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        one_of("λ\\".chars()),
        spaces().with(variable()),
        spaces().with(char('.')),
        spaces().with(expression()),
    )
        .map(|(_, input, _, body)| (input, body))
}

parser!{
    pub fn application[I]()(I) -> (Term, Term)
    where [I: Stream<Item = char>]
    {
        application_()
    }
}

fn application_<I>() -> impl Parser<Input = I, Output = (Term, Term)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        between(
            char('('),
            char(')'),
            spaces().with(expression().skip(spaces().silent())),
        ),
        spaces().with(expression()),
    )
}

pub fn variable<I>() -> impl Parser<Input = I, Output = Var>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        satisfy(|c: char| c.is_lowercase() && c != 'λ' && c != '\\')
            .expected("a lower case letter, but not 'λ' or '\'"),
        many(lower().or(digit()).or(char('\''))),
    )
        .map(|(first, mut rest): (char, String)| {
            rest.insert(0, first);
            Var(rest)
        })
}
