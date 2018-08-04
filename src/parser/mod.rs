#![allow(missing_debug_implementations)]

#[cfg(test)]
mod tests;

use combine::char::{char, spaces};
use combine::combinator::{between, choice, many, one_of};
pub use combine::{ParseError, Parser, Stream};

use syntax::{Expr, Var};

parser!{
    pub fn expression[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        expression_()
    }
}

fn expression_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        abstraction().map(|(param, body)| Expr::lam(param, body)),
        application().map(|(expr1, expr2)| Expr::app(expr1, expr2)),
        variable().map(From::from),
    )).skip(spaces())
}

parser!{
    pub fn abstraction[I]()(I) -> (Var, Expr)
    where [I: Stream<Item = char>]
    {
        abstraction_()
    }
}

fn abstraction_<I>() -> impl Parser<Input = I, Output = (Var, Expr)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (one_of("λ\\".chars()), variable(), char('.'), expression())
        .map(|(_, input, _, body)| (input, body))
}

parser!{
    pub fn application[I]()(I) -> (Expr, Expr)
    where [I: Stream<Item = char>]
    {
        application_()
    }
}

fn application_<I>() -> impl Parser<Input = I, Output = (Expr, Expr)>
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
