//! The parser that transform expressions into a `Term`.

use std::fmt::{self, Display};
use std::iter::IntoIterator;
use std::mem;

use term::{app, lam, var, Term};

use self::ParseErrorKind::*;
use self::Token::*;

/// Parses a `str` slice into `Term`.
///
/// This is a wrapper function of the more general `parse` function. It calls
/// the `parse` function by converting the `str` slice into an `Iterator` over
/// its `char`s.
pub fn parse_str(input: &str) -> Result<Term, ParseError> {
    parse_tokens(tokenize(input.chars())?)
}

/// Parses the input into a `Term`.
///
/// It returns `Ok(Term)` when the given characters can be successfully parsed
/// or `Err(ParseError)` if an error occurs.
///
/// The input can be any data structure that can be converted into an `Iterator`
/// over its `char`s.
pub fn parse(input: impl IntoIterator<Item = char>) -> Result<Term, ParseError> {
    parse_tokens(tokenize(input)?)
}

/// Converts a `str` slice into a list of `Token`s.
///
/// This is a wrapper function that calls the more general `tokenize` function
/// by converting the input `str` slice into an `Iterator` over its `char`s.
pub fn tokenize_str(input: &str) -> Result<Vec<(Token, CharPosition)>, ParseError> {
    tokenize(input.chars())
}

/// Converts a stream of `char`s into a list of `Token`s.
///
/// The returned tokens are paired with their position within the input stream.
/// Hence this function actually converts the input stream into a `Vec<(Token,
/// CharPosition)`. The position information is used for `ParseError`s found by
/// the `parse` function.
pub fn tokenize(
    input: impl IntoIterator<Item = char>,
) -> Result<Vec<(Token, CharPosition)>, ParseError> {
    let mut tokens = Vec::with_capacity(16);
    let mut position = CharPosition::default();
    let mut name_pos = position;
    let mut name = String::new();
    for chr in input {
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
                tokens.push((BodySeparator, position));
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
            '_' | '\'' => {
                if name.is_empty() {
                    return Err(ParseError::new(
                        InvalidCharacter,
                        position,
                        format!("{}", chr),
                        "any unicode alphanumeric character or one of 'λ', `\\`, '.', '(', ')'",
                        None,
                    ));
                } else {
                    name.push(chr);
                }
            },
            chr if chr.is_whitespace() => {
                if !name.is_empty() {
                    tokens.push((Identifier(name), name_pos));
                    name = String::new();
                }
            },
            chr if chr.is_alphanumeric() => {
                if name.is_empty() {
                    name_pos = position;
                }
                name.push(chr);
            },
            _ => {
                if name.is_empty() {
                    return Err(ParseError::new(
                        InvalidCharacter,
                        position,
                        format!("{}", chr),
                        "any unicode alphanumeric character or one of 'λ', `\\`, '.', '(', ')'",
                        None,
                    ));
                } else {
                    return Err(ParseError::new(
                        InvalidCharacter,
                        position,
                        format!("{}", chr),
                        "any unicode alphanumeric character or one of '_', '\\''",
                        None,
                    ));
                }
            },
        };
    }
    if !name.is_empty() {
        tokens.push((Identifier(name), name_pos));
    }
    Ok(tokens)
}

trait StringExt {
    fn first_char_matches<P>(&self, predicate: P) -> bool
    where
        P: Fn(&char) -> bool;
}

impl StringExt for String {
    fn first_char_matches<P>(&self, predicate: P) -> bool
    where
        P: Fn(&char) -> bool,
    {
        self.chars().take(1).next().filter(predicate).is_some()
    }
}

/// Start of a new sub-term with `Vec<Term>` as the parent term sequence.
enum SubTermIndicator {
    FunctionArgument(String, Vec<Term>),
    LeftParen(CharPosition, Vec<Term>),
}

/// Parses a list of `Token`s into a `Term`.
pub fn parse_tokens(
    tokens: impl IntoIterator<Item = (Token, CharPosition)>,
) -> Result<Term, ParseError> {
    use self::SubTermIndicator::*;
    let mut last_position = CharPosition::default();
    let mut subterm_indicators = Vec::with_capacity(8);
    let mut term_seq = Vec::with_capacity(16);
    let mut token_iter = tokens.into_iter();
    while let Some((token, position)) = token_iter.next() {
        last_position = position;
        match token {
            Identifier(name) => term_seq.push(var(name)),
            Lambda => {
                let token_opt = token_iter.next();
                let param = match token_opt {
                    Some((Identifier(name), _)) => name,
                    Some((token, position)) => {
                        return Err(ParseError::new(
                            AbstractionHeadExpected,
                            position,
                            token,
                            "an identifier",
                            hint("a lambda abstraction must define a bound variable in its head"),
                        ));
                    },
                    None => {
                        return Err(ParseError::new(
                            UnexpectedEndOfInput,
                            position,
                            "end of input",
                            "an identifier",
                            hint("a lambda abstraction must define a bound variable in its head"),
                        ))
                    },
                };
                let token_opt = token_iter.next();
                match token_opt {
                    Some((BodySeparator, _)) => {},
                    Some((token, position)) => {
                        return Err(ParseError::new(
                            AbstractionBodyExpected,
                            position,
                            token,
                            "the '.' character as start of the lambda body",
                            hint("a lambda abstraction must contain a body, that is an expression following the '.' character"),
                        ))
                    },
                    None => {
                        return Err(ParseError::new(
                            UnexpectedEndOfInput,
                            position,
                            "end of input",
                            "the '.' character as start of the lambda body",
                            hint("a lambda abstraction must contain a body, that is an expression following the '.' character"),
                        ))
                    }
                }
                subterm_indicators.push(FunctionArgument(
                    param,
                    mem::replace(&mut term_seq, Vec::with_capacity(8)),
                ));
            },
            LParen => {
                subterm_indicators.push(LeftParen(
                    position,
                    mem::replace(&mut term_seq, Vec::with_capacity(8)),
                ));
            },
            RParen => loop {
                if let Some(indicator) = subterm_indicators.pop() {
                    let (subterm, lparen) =
                        build_sub_term(position, &token, indicator, &mut term_seq)?;
                    term_seq.push(subterm);
                    if lparen {
                        break;
                    }
                } else {
                    return Err(ParseError::new(
                        EmptyExpression,
                        position,
                        token,
                        "a variable, a lambda abstraction or an application between parenthesis",
                        None,
                    ));
                }
            },
            BodySeparator => {
                return Err(ParseError::new(
                    UnexpectedToken,
                    position,
                    token,
                    "a variable, a lambda abstraction or an application",
                    None,
                ))
            },
        }
    }
    while let Some(indicator) = subterm_indicators.pop() {
        let (subterm, lparen) = build_sub_term(
            last_position,
            "invalid lambda expression",
            indicator,
            &mut term_seq,
        )?;
        term_seq.push(subterm);
        if lparen {
            break;
        }
    }
    if !subterm_indicators.is_empty() {
        return Err(ParseError::new(
            MissingClosingParen,
            last_position,
            "left paren",
            "matching right paren",
            None,
        ));
    }
    build_term(term_seq).ok_or_else(|| ParseError::new(
        EmptyExpression,
        CharPosition::default(),
        "invalid lambda expression",
        "at least one variable, abstraction or application",
        hint("a lambda expression must consist of at least one term, like a variable, an abstraction or an application"),
    ))
}

fn build_sub_term(
    position: CharPosition,
    token: impl Display,
    subterm_indicator: SubTermIndicator,
    term_seq: &mut Vec<Term>,
) -> Result<(Term, bool), ParseError> {
    use self::SubTermIndicator::*;
    match subterm_indicator {
        FunctionArgument(param, parent_seq) => {
            let body = build_term(mem::replace(term_seq, parent_seq)).ok_or_else(|| ParseError::new(
                EmptyAbstractionBody,
                position,
                token,
                "a variable, a lambda abstraction or an application as body of the lambda abstraction",
                None,
            ))?;
            Ok((lam(param, body), false))
        },
        LeftParen(position, parent_seq) => build_term(mem::replace(term_seq, parent_seq))
            .map(|term| (term, true))
            .ok_or_else(|| {
                ParseError::new(
                    EmptyExpression,
                    position,
                    token,
                    "a variable, a lambda abstraction or an application between parenthesis",
                    None,
                )
            }),
    }
}

fn build_term(mut term_seq: Vec<Term>) -> Option<Term> {
    if term_seq.is_empty() {
        None
    } else {
        let first_term = term_seq.remove(0);
        if term_seq.is_empty() {
            Some(first_term)
        } else {
            Some(term_seq.into_iter().fold(first_term, app))
        }
    }
}

/// A token in a lambda expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Start of a lambda abstraction
    Lambda,
    /// Separator of the body of a lambda abstraction
    BodySeparator,
    /// Opening parenthesis
    LParen,
    /// Closing parenthesis
    RParen,
    /// An identifier name
    Identifier(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let display = match *self {
            Lambda => "lambda",
            BodySeparator => "'.'",
            LParen => "'('",
            RParen => "')'",
            Identifier(_) => "identifier",
        };
        write!(f, "{}", display)
    }
}

/// Constructs a `CharPosition` from line and column.
///
/// This is a convenience function to easily construct `CharPosition`s.
pub fn pos(line: usize, column: usize) -> CharPosition {
    CharPosition {
        line,
        column,
        newline: false,
    }
}

/// Represents a position in a stream of `char`s.
///
/// The first character in a stream is at line 1 and column 1. Every newline
/// character advances the line by 1 and resets the column so that the next
/// character gets column 1 at the next line.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharPosition {
    /// the line number
    line: usize,
    /// the column number
    column: usize,
    /// the newline indicator
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
    /// Constructs a new `CharPosition` with given line and column number.
    pub fn new(line: usize, column: usize) -> Self {
        CharPosition {
            line,
            column,
            newline: false,
        }
    }

    /// Constructs a new `CharPosition` with given line and column number and
    /// an indicator, that the next character is positioned at a new line.
    pub fn with_newline(line: usize, column: usize, newline: bool) -> Self {
        CharPosition {
            line,
            column,
            newline,
        }
    }

    /// Returns the line number of this position.
    pub fn line(&self) -> usize {
        self.line
    }

    /// Returns the column number of this position.
    pub fn column(&self) -> usize {
        self.column
    }

    /// Returns the newline indicator of this position.
    pub fn newline(&self) -> bool {
        self.newline
    }

    /// Advances this position to the next character.
    ///
    /// The first character in a stream is at line 1 and column 1. Every newline
    /// character advances the line by 1 and resets the column so that the next
    /// character gets column 1 at the next line.
    pub fn next(&mut self, chr: char) {
        if self.newline {
            self.newline = false;
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        if chr == '\n' {
            self.newline = true;
        }
    }
}

/// An error that occurs during parsing of expressions.
#[cfg_attr(feature = "failure", derive(Fail))]
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    /// kind of error
    kind: ParseErrorKind,
    /// character position in the input stream where the error has been found
    position: CharPosition,
    /// the found slice of the expression that causes the error
    found: String,
    /// a description of what is expected to form a correct expression
    expected: String,
    /// an optional hint on how to fix the error
    hint: Option<Hint>,
}

impl ParseError {
    /// Constructs a new `ParseError` with the given information.
    pub fn new(
        kind: ParseErrorKind,
        position: CharPosition,
        found: impl Display,
        expected: impl Display,
        hint: Option<Hint>,
    ) -> Self {
        ParseError {
            kind,
            position,
            found: found.to_string(),
            expected: expected.to_string(),
            hint: hint.map(Into::into),
        }
    }

    /// Returns the kind of error that has been found.
    pub fn kind(&self) -> ParseErrorKind {
        self.kind
    }

    /// Returns the position in the input stream, where the error has been
    /// found.
    pub fn position(&self) -> CharPosition {
        self.position
    }

    /// Returns the slice of the expression which causes the error.
    pub fn found(&self) -> &str {
        &self.found
    }

    /// Returns a description of what is expected to form a correct expression.
    pub fn expected(&self) -> &str {
        &self.expected
    }

    /// Returns an additional hint for finding and fixing the error.
    ///
    /// The hint is optional. So some errors might not have a hint for you.
    pub fn hint(&self) -> Option<&Hint> {
        self.hint.as_ref()
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[{}]: {}", self.kind, self.kind.explain())?;
        writeln!(f, "   {}   \t   found:\t{}", self.position, self.found)?;
        writeln!(f, "        \texpected:\t{}", self.expected)?;
        if let Some(ref hint) = self.hint {
            writeln!(f, "   hint: {}", hint)
        } else {
            write!(f, "")
        }
    }
}

/// The kind of a parse error.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParseErrorKind {
    /// The expression is empty.
    EmptyExpression,
    /// Expected an identifier at this position.
    IdentifierExpected,
    /// Found a character that is invalid at this position.
    InvalidCharacter,
    /// Expected a lambda abstraction body separator at this position.
    AbstractionBodyExpected,
    /// Expected a lambda abstraction head at this position.
    AbstractionHeadExpected,
    /// The lambda abstraction body is empty.
    EmptyAbstractionBody,
    /// Found a closing parenthesis without a matching opening one.
    MissingOpeningParen,
    /// Missing a closing parenthesis for a found opening one.
    MissingClosingParen,
    /// End of input before a term is complete.
    UnexpectedEndOfInput,
    /// Unexpected token at this position.
    UnexpectedToken,
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ParseErrorKind {
    /// Returns an explanation for this kind of parse error.
    pub fn explain(self) -> &'static str {
        match self {
            EmptyExpression => "can not parse empty expression",
            IdentifierExpected => "expected a valid identifier",
            InvalidCharacter => "invalid character found",
            AbstractionBodyExpected => {
                "expected a '.' character as start of the body of the lambda abstraction"
            },
            AbstractionHeadExpected => {
                "expected an identifier as bound variable in the lambda abstraction"
            },
            EmptyAbstractionBody => "the lambda abstraction body is empty",
            MissingClosingParen => "opening parenthesis without a matching closing one",
            MissingOpeningParen => "closing parenthesis without a matching opening one",
            UnexpectedEndOfInput => "unexpected end of input",
            UnexpectedToken => "unexpected token found",
        }
    }
}

/// A hint how to avoid an error.
#[derive(Debug, Clone, PartialEq)]
pub struct Hint(String);

impl Display for Hint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Constructs a `Hint` from a type that can be converted into a `String`.
pub fn hint(text: impl Into<String>) -> Option<Hint> {
    Some(Hint(text.into()))
}

#[cfg(test)]
mod tests;
