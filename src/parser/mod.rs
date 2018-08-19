#[cfg(test)]
mod tests;

use std::fmt::{self, Display};
use std::iter::IntoIterator;

use term::{Term, Var};

use self::ErrorKind::*;
use self::Token::*;

pub fn parse_str(input: &str) -> Result<Term, ParseError> {
    parse_tokens(tokenize(input.chars())?)
}

pub fn parse(input: impl IntoIterator<Item = char>) -> Result<Term, ParseError> {
    parse_tokens(tokenize(input)?)
}

pub fn tokenize_str(input: &str) -> Result<Vec<(Token, CharPosition)>, ParseError> {
    tokenize(input.chars())
}

pub fn tokenize(
    input: impl IntoIterator<Item = char>,
) -> Result<Vec<(Token, CharPosition)>, ParseError> {
    let mut tokens = Vec::with_capacity(16);
    let mut position = CharPosition::default();
    let mut name_pos = position;
    let mut name = String::new();
    let mut char_iter = input.into_iter();
    while let Some(chr) = char_iter.next() {
        position.next(chr);
        match chr {
            'λ' | '\\' => {
                if !name.is_empty() {
                    tokens.push((Identifier(name), name_pos));
                    name = String::new();
                }
                tokens.push((Lambda, position));
            },
            '.' => {
                if !name.is_empty() {
                    tokens.push((Identifier(name), name_pos));
                    name = String::new();
                }
                tokens.push((Dot, position));
            },
            '(' => {
                if !name.is_empty() {
                    tokens.push((Identifier(name), name_pos));
                    name = String::new();
                }
                tokens.push((LParen, position));
            },
            ')' => {
                if !name.is_empty() {
                    tokens.push((Identifier(name), name_pos));
                    name = String::new();
                }
                tokens.push((RParen, position));
            },
            '_' | '\'' => if name.is_empty() {
                return Err(ParseError::new(
                    InvalidCharacter,
                    position,
                    format!("{}", chr),
                    "any lower case letter or 'λ', '.', '(', ')'",
                    "",
                ));
            } else {
                if name.is_empty() {
                    name_pos = position;
                }
                name.push(chr);
            },
            chr if chr.is_whitespace() => if !name.is_empty() {
                tokens.push((Identifier(name), name_pos));
                name = String::new();
            },
            chr if chr.is_lowercase() => {
                if name.is_empty() {
                    name_pos = position;
                }
                name.push(chr);
            },
            chr if chr.is_ascii_digit() => if name.is_empty() {
                return Err(ParseError::new(
                    InvalidCharacter,
                    position,
                    format!("{}", chr),
                    "any lower case letter or 'λ', '.', '(', ')'",
                    "",
                ));
            } else {
                if name.is_empty() {
                    name_pos = position;
                }
                name.push(chr);
            },
            _ => {
                return Err(ParseError::new(
                    InvalidCharacter,
                    position,
                    format!("{}", chr),
                    "any lower case letter, digit or 'λ', '.', '(', ')', '_', '\''",
                    "",
                ))
            },
        };
    }
    if !name.is_empty() {
        tokens.push((Identifier(name), name_pos));
    }
    Ok(tokens)
}

pub fn parse_tokens(
    tokens: impl IntoIterator<Item = (Token, CharPosition)>,
) -> Result<Term, ParseError> {
    let mut ctx = Context::new();
    let parsed_term = parse_tokens_rec(tokens.into_iter(), &mut ctx).map(|(term, _)| term);
    if !ctx.lparens.is_empty() {
        return Err(ParseError::new(
            MissingClosingParen,
            ctx.lparens.pop().expect("just checked for non empty"),
            "left paren",
            "matching right paren",
            "",
        ));
    }
    parsed_term
}

fn parse_tokens_rec<I>(mut token_iter: I, ctx: &mut Context) -> Result<(Term, I), ParseError>
where
    I: Iterator<Item = (Token, CharPosition)>,
{
    let mut term_seq = Vec::with_capacity(8);
    while let Some((token, position)) = token_iter.next() {
        match token {
            Identifier(name) => term_seq.push(Term::var(name)),
            Lambda => {
                let token_opt = token_iter.next();
                let name = match token_opt {
                    Some((Identifier(name), _)) => name,
                    Some((token, position)) => {
                        return Err(ParseError::new(
                            LambdaHeadExpected,
                            position,
                            token,
                            "an identifier",
                            "a lambda expression must define a bound variable in its head",
                        ));
                    },
                    None => {
                        return Err(ParseError::new(
                            UnexpectedEndOfInput,
                            position,
                            "end of input",
                            "an identifier",
                            "a lambda expression must define a bound variable in its head",
                        ))
                    },
                };
                let token_opt = token_iter.next();
                match token_opt {
                    Some((Dot, _)) => {},
                    Some((token, position)) => {
                        return Err(ParseError::new(
                            LambdaBodyExpected,
                            position,
                            token,
                            "the '.' character as start of the lambda body",
                            "a lambda abstraction must contain a body, that is an expression following the '.' character",
                        ))
                    },
                    None => {
                        return Err(ParseError::new(
                            UnexpectedEndOfInput,
                            position,
                            "end of input",
                            "the '.' character as start of the lambda body",
                            "a lambda abstraction must contain a body, that is an expression following the '.' character",
                        ))
                    }
                }
                let num_lparens = ctx.lparens.len();
                let (body, rest) = parse_tokens_rec(token_iter, ctx)?;
                token_iter = rest;
                term_seq.push(Term::lam(Var(name), body));
                if num_lparens > ctx.lparens.len() {
                    break;
                }
            },
            LParen => {
                ctx.lparens.push(position);
                let (subtree, rest) = parse_tokens_rec(token_iter, ctx)?;
                token_iter = rest;
                term_seq.push(subtree);
            },
            RParen => {
                if ctx.lparens.is_empty() {
                    return Err(ParseError::new(
                        MissingOpeningParen,
                        position,
                        "right paren",
                        "matching left paren",
                        "",
                    ));
                }
                let _ = ctx.lparens.pop();
                break;
            },
            _ => {
                return Err(ParseError::new(
                    UnexpectedToken,
                    position,
                    token,
                    "a variable, a lambda abstraction or an application",
                    "",
                ))
            },
        }
    }
    if term_seq.is_empty() {
        Err(ParseError::new(EmptyExpression, CharPosition::default(), "invalid lambda expression", "at least one variable, abstraction or application", "a lambda expression must consist of at least one term, like a variable, an abstraction or an application"))
    } else {
        let first_term = term_seq.remove(0);
        if term_seq.is_empty() {
            Ok((first_term, token_iter))
        } else {
            let term = term_seq
                .into_iter()
                .fold(first_term, |acc, expr2| Term::app(acc, expr2));
            Ok((term, token_iter))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Lambda,
    Dot,
    LParen,
    RParen,
    Identifier(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let display = match *self {
            Lambda => "lambda",
            Dot => "'.'",
            LParen => "'('",
            RParen => "')'",
            Identifier(_) => "identifier",
        };
        write!(f, "{}", display)
    }
}

#[cfg(test)]
pub fn pos(line: usize, column: usize) -> CharPosition {
    CharPosition {
        line,
        column,
        newline: false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharPosition {
    line: usize,
    column: usize,
    newline: bool,
}

impl Default for CharPosition {
    fn default() -> Self {
        CharPosition {
            line: 0,
            column: 0,
            newline: true,
        }
    }
}

impl Display for CharPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl CharPosition {
    pub fn new(line: usize, column: usize) -> Self {
        CharPosition {
            line,
            column,
            newline: false,
        }
    }

    pub fn with_newline(line: usize, column: usize, newline: bool) -> Self {
        CharPosition {
            line,
            column,
            newline,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn newline(&self) -> bool {
        self.newline
    }

    pub fn next(&mut self, chr: char) {
        if self.newline {
            self.newline = false;
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        match chr {
            '\n' => {
                self.newline = true;
            },
            _ => {},
        }
    }
}

struct Context {
    lparens: Vec<CharPosition>,
}

impl Context {
    fn new() -> Self {
        Context {
            lparens: Vec::with_capacity(8),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    kind: ErrorKind,
    position: CharPosition,
    found: String,
    expected: String,
    hint: String,
}

impl ParseError {
    pub fn new(
        kind: ErrorKind,
        position: CharPosition,
        found: impl Display,
        expected: impl Display,
        hint: impl Into<String>,
    ) -> Self {
        ParseError {
            kind,
            position,
            found: found.to_string(),
            expected: expected.to_string(),
            hint: hint.into(),
        }
    }

    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn position(&self) -> CharPosition {
        self.position
    }

    pub fn found(&self) -> &str {
        &self.found
    }

    pub fn expected(&self) -> &str {
        &self.expected
    }

    pub fn hint(&self) -> &str {
        &self.hint
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[{}]: {}", self.kind, self.kind.explain())?;
        writeln!(f, "   {}   \t   found:\t{}", self.position, self.found)?;
        writeln!(f, "        \texpected:\t{}", self.expected)?;
        if !self.hint.is_empty() {
            writeln!(f, "   hint: {}", self.hint)
        } else {
            write!(f, "")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorKind {
    EmptyExpression,
    IdentifierExpected,
    InvalidCharacter,
    LambdaBodyExpected,
    LambdaHeadExpected,
    MissingOpeningParen,
    MissingClosingParen,
    UnexpectedEndOfInput,
    UnexpectedToken,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ErrorKind {
    pub fn explain(&self) -> &str {
        match *self {
            EmptyExpression => "can not parse empty expression",
            IdentifierExpected => "expected a valid identifier",
            InvalidCharacter => "invalid character found",
            LambdaBodyExpected => {
                "expected a '.' character as start of the body of the lambda abstraction"
            },
            LambdaHeadExpected => {
                "expected an identifier as bound variable in the lambda abstraction"
            },
            MissingClosingParen => "opening parenthesis without a matching closing one",
            MissingOpeningParen => "closing parenthesis without a matching opening one",
            UnexpectedEndOfInput => "unexpected end of input",
            UnexpectedToken => "unexpected token found",
        }
    }
}
