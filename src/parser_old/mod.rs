//! Syntax:
//!
//! λx.x

#[cfg(test)]
mod tests;

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::{self, Debug, Display};

use syntax::Term;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    /// the lambda symbol ('λ' or '\')
    Lambda,
    /// left parenthesis
    LParen,
    /// right parenthesis
    RParen,
    /// the dot
    Dot,
    /// an alphabetic character
    Alphabetic(char),
    /// a whitespace character
    Whitespace,
}
//
//pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
//    let mut tokens = Vec::new();
//    let mut chars = input.chars().enumerate();
//    while let Some((index, character)) = chars.next() {
//        use self::Token::*;
//        match character {
//            '\\' | 'λ' => tokens.push(Lambda),
//            '(' => tokens.push(LParen),
//            ')' => tokens.push(RParen),
// _ if character.is_alphabetic() =>
// tokens.push(Alphabetic(character)), _ if
// character.is_whitespace() => tokens.push(Whitespace), _ => return
// Err(ParseError::InvalidCharacter(character, index + 1)),        }
//    }
//    Ok(tokens)
//}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParseError {
    InvalidCharacter(char, SourcePosition),
    UnexpectedCharacter(char, &'static str, SourcePosition),
    UnexpectedEndOfInput(SourcePosition),
}

impl From<EndOfInput> for ParseError {
    fn from(eoi: EndOfInput) -> Self {
        ParseError::UnexpectedEndOfInput(eoi.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EndOfInput(SourcePosition);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourcePosition {
    line: usize,
    column: usize,
}

impl Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Ord for SourcePosition {
    fn cmp(&self, other: &Self) -> Ordering {
        use self::Ordering::*;
        match self.line.cmp(&other.line) {
            Equal => self.column.cmp(&other.column),
            line_cmp => line_cmp,
        }
    }
}

impl PartialOrd for SourcePosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct CharStream {
    line: usize,
    column: usize,
    new_line: bool,
    buffer: Vec<char>,
    source: Box<Iterator<Item = char>>,
}

impl Debug for CharStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "CharStream {{ line: {:?}, column: {:?}, new_line: {:?}, buffer: {:?}, source: ...}}",
            self.line, self.column, self.new_line, self.buffer
        )
    }
}

impl CharStream {
    pub fn new<I>(chars: I) -> Self
    where
        I: 'static + Iterator<Item = char>,
    {
        let char_iter = chars.into_iter();
        let (min_size, _) = char_iter.size_hint();
        let buffer_size = min_size.min(16).max(128);
        CharStream {
            line: 0,
            column: 0,
            new_line: true,
            source: Box::new(char_iter),
            buffer: Vec::with_capacity(buffer_size),
        }
    }

    pub fn position(&self) -> SourcePosition {
        SourcePosition {
            line: self.line,
            column: self.column,
        }
    }

    pub fn uncons(&mut self) -> Result<char, EndOfInput> {
        if let Some(chr) = self.buffer.first() {
            if self.new_line {
                self.new_line = *chr == '\n';
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            Ok(*chr)
        } else {
            Err(EndOfInput(self.position()))
        }
    }
}

pub trait Parser {
    type Output;
    fn parse(&mut self, input: CharStream) -> Result<(Self::Output, CharStream), ParseError>;
}

#[derive(Debug, Clone, Copy)]
pub struct TermParser;

impl Parser for TermParser {
    type Output = Term;

    fn parse(
        &mut self,
        input: CharStream,
    ) -> Result<(<Self as Parser>::Output, CharStream), ParseError> {
        LambdaParser
            .parse(input)
            .and_then(|(_, input)| VariableParser.parse(input))
            .map(|(var_name, input)| (Term::Variable(var_name), input))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LambdaParser;

impl Parser for LambdaParser {
    type Output = ();

    fn parse(
        &mut self,
        mut input: CharStream,
    ) -> Result<(<Self as Parser>::Output, CharStream), ParseError> {
        match input.uncons()? {
            '\\' | 'λ' => Ok(((), input)),
            chr => Err(ParseError::UnexpectedCharacter(
                chr,
                "['λ', '\\']",
                input.position(),
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DotParser;

impl Parser for DotParser {
    type Output = ();

    fn parse(
        &mut self,
        mut input: CharStream,
    ) -> Result<(<Self as Parser>::Output, CharStream), ParseError> {
        match input.uncons()? {
            '.' => Ok(((), input)),
            chr => Err(ParseError::UnexpectedCharacter(
                chr,
                "['.']",
                input.position(),
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VariableParser;

impl Parser for VariableParser {
    type Output = String;

    fn parse(
        &mut self,
        mut input: CharStream,
    ) -> Result<(<Self as Parser>::Output, CharStream), ParseError> {
        let mut var_name = String::new();
        match input.uncons()? {
            chr if chr.is_alphabetic() => var_name.push(chr),
            chr => {
                return Err(ParseError::UnexpectedCharacter(
                    chr,
                    "at least one alphabetic character",
                    input.position(),
                ))
            },
        }
        while let Ok(chr) = input.uncons() {
            match chr {
                _ if chr.is_alphanumeric() => var_name.push(chr),
                _ => break,
            }
        }
        Ok((var_name, input))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct WhitespaceParser;

impl Parser for WhitespaceParser {
    type Output = ();

    fn parse(
        &mut self,
        mut input: CharStream,
    ) -> Result<(<Self as Parser>::Output, CharStream), ParseError> {
        match input.uncons()? {
            chr if chr.is_whitespace() => Ok(((), input)),
            chr => Err(ParseError::UnexpectedCharacter(
                chr,
                "a whitespace character",
                input.position(),
            )),
        }
    }
}
