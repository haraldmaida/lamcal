use failure::Error;

use lamcal::{parse_str, Binding, Term, VarName};

use command::{cont_output, Command, Continuation};
use context::Context;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LetStatementPart {
    Undefined,
    Identifier,
    Expression,
}

impl Default for LetStatementPart {
    fn default() -> Self {
        LetStatementPart::Undefined
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    identifier: String,
    expression: Term,
}

impl LetStatement {
    pub fn unwrap(self) -> (String, Term) {
        (self.identifier, self.expression)
    }
}

impl Into<Binding> for LetStatement {
    fn into(self) -> Binding {
        Binding::new(VarName(self.identifier), self.expression)
    }
}

impl Command for LetStatement {
    type Input = Self;
    type Output = String;

    fn with_input(input: <Self as Command>::Input) -> Self {
        input
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        let message = format!("Bound: {} => {}", &self.identifier, &self.expression);
        ctx.env_mut().insert_binding(self.into());
        cont_output(message, "")
    }
}

pub fn parse_let_statement(line: &str) -> Result<LetStatement, Error> {
    let mut identifier = String::new();
    let mut expression = String::new();
    let mut part = if line.starts_with(":let ") {
        LetStatementPart::Identifier
    } else {
        LetStatementPart::Undefined
    };
    let mut chr_iter = line.chars().skip(5);
    while let Some(chr) = chr_iter.next() {
        match chr {
            '=' => {
                part = LetStatementPart::Expression;
            },
            _ => match part {
                LetStatementPart::Identifier => {
                    if !chr.is_whitespace() {
                        identifier.push(chr);
                    }
                },
                LetStatementPart::Expression => {
                    expression.push(chr);
                },
                LetStatementPart::Undefined => {
                    if !chr.is_whitespace() {
                        return Err(format_err!("Character not allowed: {}", chr));
                    }
                },
            },
        }
    }
    if identifier.contains(char::is_whitespace) {
        return Err(format_err!("Invalid identifier: {}", identifier));
    }
    let expression = parse_str(&expression)?;
    Ok(LetStatement {
        identifier,
        expression,
    })
}
