use std::ascii::AsciiExt;
use std::str::CharIndices;

use self::ErrorCode::*;
use self::Tok::*;

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: usize,
    pub code: ErrorCode
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
    UnterminatedStringLiteral,
    UnterminatedOperator,
}

fn error<T>(c: ErrorCode, l: usize) -> Result<T,Error> {
    Err(Error { location: l, code: c })
}

#[derive(Clone, Debug)]
pub struct FortranUserStr<'input> {
    string: &'input str
}

impl<'input> FortranUserStr<'input> {
    pub fn new(string: &'input str) -> FortranUserStr {
        debug_assert!(string.is_ascii());

        FortranUserStr { string: string }
    }

    pub fn as_str(&self) -> &str {
        self.string
    }
}

impl<'input> PartialEq for FortranUserStr<'input> {
    fn eq(&self, other: &FortranUserStr) -> bool {
        self.as_str().eq_ignore_ascii_case(other.as_str())
    }
}

impl<'input> Eq for FortranUserStr<'input> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok<'input> {
    // statements
    Program,
    End,
    
    // Actions
    Print,

    // user strings
    Id(FortranUserStr<'input>),
    IntegerLiteralConstant(&'input str),
    CharLiteralConstant(&'input str),
    DefinedOperator(FortranUserStr<'input>),

    // Symbols
    And,
    Equivalent,
    NotEquivalent,
    Not,
    Or,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
    True,
    False,

    Plus,
    Minus,
    Slash,
    SlashSlash,
    Star,
    StarStar,

    // structure
    NewLine,
    SemiColon,
    Comma,

    LeftParen,
    RightParen,
}

pub struct Tokenizer<'input> {
    text: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

pub type Spanned<T> = (usize, T, usize);

const KEYWORDS: &'static [(&'static str, Tok<'static>)] = &[
    ("PROGRAM", Program),
    ("END",     End),
    ("PRINT",   Print),
    ];

const INTRINSIC_OPERATORS: &'static [(&'static str, Tok<'static>)] = &[
    ("AND",   And),
    ("EQV",   Equivalent),
    ("NEQV",  NotEquivalent),
    ("NOT",   Not),
    ("OR",    Or),
    ("EQ",    Equals),
    ("NE",    NotEquals),
    ("LT",    LessThan),
    ("LE",    LessThanOrEquals),
    ("GT",    GreaterThan),
    ("GE",    GreaterThanOrEquals),
    ("TRUE",  True),
    ("FALSE", False)
    ];

impl<'input> Tokenizer<'input> {
    pub fn new(text: &'input str) -> Tokenizer<'input> {
        let mut t = Tokenizer {
            text: text,
            chars: text.char_indices(),
            lookahead: None,
        };
        t.bump();
        t
    }

    fn operator(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        match self.take_while(is_operator_continue) {
            Some(idx1) => {
                match self.lookahead {
                    Some((_, '.')) => {
                        // consume .
                        self.bump();

                        // don't include . in operator name
                        let operator = &self.text[idx0+1..idx1];

                        let tok =
                            INTRINSIC_OPERATORS
                                .iter()
                                .filter(|&&(w, _)| {
                                    w.eq_ignore_ascii_case(operator)
                                })
                                .map(|&(_, ref t)| t.clone())
                                .next()
                                .unwrap_or_else(|| {
                                    DefinedOperator(FortranUserStr::new(operator))
                                });

                        Ok((idx0, tok, idx1+1))
                    }
                    _ => error(UnterminatedOperator, idx0)
                }
            }
            None => error(UnterminatedOperator, idx0)
        }
    }

    fn identifierish(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        let (start, word, end) = self.word(idx0);

        let tok =
            KEYWORDS
                .iter()
                .filter(|&&(w, _)| w.eq_ignore_ascii_case(word))
                .map(|&(_, ref t)| t.clone())
                .next()
                .unwrap_or_else(|| {
                    Id(FortranUserStr::new(word))
                });

        Ok((start, tok, end))
    }

    fn word(&mut self, idx0: usize) -> Spanned<&'input str> {
        match self.take_while(is_identifier_continue) {
            Some(end) => (idx0, &self.text[idx0..end], end),
            None => (idx0, &self.text[idx0..], self.text.len()),
        }
    }

    fn string_literal(&mut self, idx0: usize, quote: char) -> Result<Spanned<Tok<'input>>, Error> {
        let mut escape = false;
        let terminate = |c: char| {
            if escape {
                escape = false;
                false
            } else if c == '\\' {
                escape = true;
                false
            } else if c == quote {
                true
            } else {
                false
            }
        };
        match self.take_until(terminate) {
            Some(idx1) => {
                self.bump(); // consume the closing quote
                let text = &self.text[idx0+1..idx1]; // do not include quotes in the str
                Ok((idx0, CharLiteralConstant(text), idx1+1))
            }
            None => error(UnterminatedStringLiteral, idx0)
        }
    }

    fn internal_next(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '+')) => {
                    self.bump();
                    Some(Ok((idx0, Plus, idx0+1)))
                }
                Some((idx0, '-')) => {
                    self.bump();
                    Some(Ok((idx0, Minus, idx0+1)))
                }
                Some((idx0, '*')) => {
                    match self.bump() {
                        Some((idx1, '*')) => {
                            self.bump();
                            Some(Ok((idx0, StarStar, idx1+1)))
                        }
                        _ =>
                            Some(Ok((idx0, Star, idx0+1)))
                    }
                }
                Some((idx0, '/')) => {
                    match self.bump() {
                        Some((idx1, '/')) => {
                            self.bump();
                            Some(Ok((idx0, SlashSlash, idx1+1)))
                        }
                        _ =>
                            Some(Ok((idx0, Slash, idx0+1)))
                    }
                }
                Some((idx0, '(')) => {
                    self.bump();
                    Some(Ok((idx0, LeftParen, idx0+1)))
                }
                Some((idx0, ')')) => {
                    self.bump();
                    Some(Ok((idx0, RightParen, idx0+1)))
                }
                Some((idx0, '.')) => {
                    self.bump();
                    Some(self.operator(idx0))
                }
                Some((idx0, ';')) => {
                    self.bump();
                    Some(Ok((idx0, SemiColon, idx0+1)))
                }
                Some((idx0, ',')) => {
                    self.bump();
                    Some(Ok((idx0, Comma, idx0+1)))
                }
                Some((idx0, c)) if (c == '"' || c == '\'') => {
                    self.bump();
                    Some(self.string_literal(idx0, c))
                }
                // Handle LF
                Some((idx0, '\n')) => {
                    self.bump();
                    Some(Ok((idx0, NewLine, idx0+1)))
                }
                // Handle CR+CRLF
                Some((idx0, '\r')) => {
                    self.bump();
                    match self.lookahead {
                        // CRLF
                        Some((_, '\n')) => {
                            self.bump();
                            Some(Ok((idx0, NewLine, idx0+2)))
                        }
                        // CR
                        _ => Some(Ok((idx0, NewLine, idx0+1)))
                    }
                }
                Some((idx0, c)) if is_identifier_start(c) => {
                    self.bump();
                    Some(self.identifierish(idx0))
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some((idx, _)) => {
                    Some(error(UnrecognizedToken, idx))
                }
                None => {
                    None
                }
            };
        }
    }

    fn take_while<F>(&mut self, mut keep_going: F) -> Option<usize>
        where F: FnMut(char) -> bool
    {
        self.take_until(|c| !keep_going(c))
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Option<usize>
        where F: FnMut(char) -> bool
    {
        loop {
            match self.lookahead {
                None => {
                    return None;
                }
                Some((idx1, c)) => {
                    if terminate(c) {
                        return Some(idx1);
                    } else {
                        self.bump();
                    }
                }
            }
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        self.lookahead = self.chars.next();
        self.lookahead
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Spanned<Tok<'input>>, Error>;

    fn next(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        self.internal_next()
    }
}

// assumed that starting . has been consumed
// the final character will need to be validated as a . and consumed
fn is_operator_continue(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_identifier_start(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_identifier_continue(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')
}