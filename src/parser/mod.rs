#![allow(missing_debug_implementations)]

#[cfg(test)]
mod tests;

use combine::char::{char, spaces};
use combine::combinator::{between, choice, many, one_of};
pub use combine::{ParseError, Parser, Stream};

use term::{Term, Var};

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
    choice((
        abstraction().map(|(param, body)| Term::lam(param, body)),
        application().map(|(expr1, expr2)| Term::app(expr1, expr2)),
        variable().map(From::from),
    )).skip(spaces())
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
    (one_of("λ\\".chars()), variable(), char('.'), expression())
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
        between(char('('), char(')'), expression()),
        spaces().with(expression()),
    )
}

pub fn variable<I>() -> impl Parser<Input = I, Output = Var>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        one_of("abcdefghijklmnopqrstuvwxyz".chars()),
        many(one_of("abcdefghijklmnopqrstuvwxyz0123456789'".chars())),
    )
        .map(|(first, mut rest): (char, String)| {
            rest.insert(0, first);
            Var(rest)
        })
}
