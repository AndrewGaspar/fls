use std::slice;
use std::ascii::AsciiExt;
use std::iter::Iterator;
use std::str::CharIndices;

use self::ErrorCode::*;
use self::Tok::*;
use self::TakeUntil::*;
use self::Lookahead::*;

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: usize,
    pub code: ErrorCode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
    UnterminatedStringLiteral,
    UnterminatedOperator,
    UnterminatedContinuationLine,
    InvalidCarriageReturn,
    UnexpectedToken,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TakeUntil {
    Continue,
    Stop,
    Error(ErrorCode),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Lookahead {
    Character(char),
    EOF
}

fn error<T>(c: ErrorCode, l: usize) -> Result<T, Error> {
    Err(Error {
        location: l,
        code: c,
    })
}

#[derive(Clone, Debug)]
pub struct UserStr<'input> {
    string: &'input str,
}

impl<'input> UserStr<'input> {
    pub fn new(string: &'input str) -> UserStr {
        debug_assert!(string.is_ascii());

        UserStr { string: string }
    }

    pub fn iter(&self) -> UserStrIterator<'input> {
        UserStrIterator::new(self.string.as_bytes().iter())
    }
}

impl<'input> PartialEq for UserStr<'input> {
    fn eq(&self, other: &UserStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for UserStr<'input> {}

#[derive(Clone, Debug)]
pub struct UserStrIterator<'input> {
    str_iter: slice::Iter<'input, u8>,
}

impl<'input> UserStrIterator<'input> {
    fn new(str_iter: slice::Iter<'input, u8>) -> UserStrIterator<'input> {
        UserStrIterator { str_iter: str_iter }
    }
}

// Iterator over a FortranUserStr. Ignores continuation. This allows us to
// tokenize the FORTRAN program without allocating any memory.
impl<'input> Iterator for UserStrIterator<'input> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        // if we're here, we can assume that the string is already a
        // valid identifier, which means the continuation is properly
        // terminated. Just continue until we see a closing ampersand.
        loop {
            return match self.str_iter.next() {
                Some(amp) if *amp == b'&' => {
                    while b'&' != *self.str_iter.next().unwrap() {}
                    continue;
                }
                Some(x) => Some(*x),
                None => None,
            };
        }
    }
}

#[derive(Clone, Debug)]
pub struct CaseInsensitiveUserStr<'input> {
    user_str: UserStr<'input>,
}

impl<'input> CaseInsensitiveUserStr<'input> {
    pub fn new(string: &'input str) -> CaseInsensitiveUserStr {
        CaseInsensitiveUserStr { user_str: UserStr::new(string) }
    }

    pub fn iter(&self) -> UserStrIterator<'input> {
        self.user_str.iter()
    }
}

impl<'input> PartialEq for CaseInsensitiveUserStr<'input> {
    fn eq(&self, other: &CaseInsensitiveUserStr) -> bool {
        let to_lower = |c: u8| c.to_ascii_lowercase();

        self.iter().map(&to_lower).eq(other.iter().map(&to_lower))
    }
}

impl<'input> Eq for CaseInsensitiveUserStr<'input> {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok<'input> {
    // statements
    Program,
    End,

    // Actions
    Print,

    // user strings
    Id(CaseInsensitiveUserStr<'input>),
    IntegerLiteralConstant(UserStr<'input>),
    CharLiteralConstant(UserStr<'input>),
    DigitString(UserStr<'input>),
    DefinedOperator(CaseInsensitiveUserStr<'input>),

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

const KEYWORDS: &'static [(&'static str, Tok<'static>)] =
    &[("PROGRAM", Program), ("END", End), ("PRINT", Print)];

const INTRINSIC_OPERATORS: &'static [(&'static str, Tok<'static>)] = &[
    ("AND", And),
    ("EQV", Equivalent),
    ("NEQV", NotEquivalent),
    ("NOT", Not),
    ("OR", Or),
    ("EQ", Equals),
    ("NE", NotEquals),
    ("LT", LessThan),
    ("LE", LessThanOrEquals),
    ("GT", GreaterThan),
    ("GE", GreaterThanOrEquals),
    ("TRUE", True),
    ("FALSE", False),
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
        let terminate = |lookahead: Lookahead|
            match lookahead {
                Character(c) if is_operator_continue(c) => Continue,
                Character('.') => Stop,
                _ => Error(UnrecognizedToken),
            };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
                // consume .
                self.bump();

                // don't include . in operator name
                let operator = CaseInsensitiveUserStr::new(&self.text[idx0 + 1..idx1]);

                let tok = INTRINSIC_OPERATORS
                    .iter()
                    .filter(|&&(w, _)| CaseInsensitiveUserStr::new(w) == operator)
                    .map(|&(_, ref t)| t.clone())
                    .next()
                    .unwrap_or_else(|| DefinedOperator(operator));

                Ok((idx0, tok, idx1 + 1))
            }
            Some(Err(err)) => Err(err),
            None => error(UnrecognizedToken, idx0),
        }
    }

    fn identifierish(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        let terminate = |lookahead: Lookahead|
            match lookahead {
                Character(c) if is_identifier_continue(c) => Continue,
                _ => Stop,
            };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
                let word = CaseInsensitiveUserStr::new(&self.text[idx0..idx1]);

                let tok = KEYWORDS
                    .iter()
                    .filter(|&&(w, _)| CaseInsensitiveUserStr::new(w) == word)
                    .map(|&(_, ref t)| t.clone())
                    .next()
                    .unwrap_or_else(|| Id(word));

                Ok((idx0, tok, idx1))
            }
            Some(Err(err)) => Err(err),
            None => error(UnrecognizedToken, idx0),
        }
    }

    fn string_literal(&mut self, idx0: usize, quote: char) -> Result<Spanned<Tok<'input>>, Error> {
        let mut escape = false;
        let terminate = |lookahead: Lookahead| {
            if escape {
                escape = false;
                Continue
            } else {
                match lookahead {
                    Character('\\') => {
                        escape = true;
                        Continue
                    }
                    Character(c) if c == quote => Stop,
                    Character(c) if is_new_line_start(c) =>
                        Error(UnterminatedStringLiteral),
                    Character(_) => Continue,
                    EOF => Error(UnterminatedStringLiteral),
                }
            }
        };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
                self.bump(); // consume the closing quote
                // do not include quotes in the str
                let text = UserStr::new(&self.text[idx0 + 1..idx1]);
                Ok((idx0, CharLiteralConstant(text), idx1 + 1))
            }
            Some(Err(err)) => Err(err),
            None => error(UnterminatedStringLiteral, idx0),
        }
    }

    fn digit_string(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        let terminate = |lookahead: Lookahead|
            match lookahead {
                Character(c) if is_digit(c) => Continue,
                _ => Stop,
            };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
                Ok((idx0, DigitString(UserStr::new(&self.text[idx0..idx1])), idx1 + 1))
            }
            Some(Err(err)) => Err(err),
            None => error(UnterminatedStringLiteral, idx0),
        }
    }

    // expected that last seen character was '!'
    // returns nothing - merely advances to end of comment.
    fn commentary(&mut self) {
        loop {
            match self.lookahead {
                Some((_, c)) if is_new_line_start(c) => return,
                None => return,
                Some(_) => {
                    self.bump();
                    continue;
                }
            }
        }
    }

    // call when you want to consume a new-line token. Test if at the start of
    // a new line with is_new_line_start.
    fn consume_new_line(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '\n')) => {
                    self.bump();
                    Some(Ok((idx0, NewLine, idx0 + 1)))
                }
                Some((idx0, '\r')) => {
                    match self.bump() {
                        Some((_, '\n')) => {
                            self.bump();
                            Some(Ok((idx0, NewLine, idx0 + 2)))
                        }
                        // CR is not a supported line ending
                        _ => Some(error(InvalidCarriageReturn, idx0)),
                    }
                }
                Some((idx0, _)) => {
                    // this function shouldn't have been called if not currently
                    // at the start of a newline
                    assert!(false);
                    Some(error(UnrecognizedToken, idx0))
                }
                None => None,
            };
        }
    }

    fn continuation(&mut self, idx0: usize) -> Option<Result<(), Error>> {
        let mut first_line = true;

        loop {
            return match self.lookahead {
                Some((_, '!')) => {
                    self.bump();
                    self.commentary();
                    continue;
                }
                Some((_, c)) if is_new_line_start(c) => {
                    first_line = false;
                    match self.consume_new_line() {
                        // propagate errors
                        Some(Err(err)) => Some(Err(err)),
                        // discard newline token inside of continuation.
                        // EOF ends continuation
                        Some(Ok(_)) | None => continue,
                    }
                }
                Some((_, s)) if s.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some((idx1, _)) if first_line => Some(error(UnexpectedToken, idx1)),
                // If an & is encountered, we're done processing the
                // continuation. The caller should continue whatever
                // tokenization process it was previously performing.
                Some((_, '&')) => {
                    self.bump();
                    Some(Ok(()))
                }
                // If a new token is encountered, then the continuation has
                // ended. The new character is a new token.
                Some(_) => None,
                None => Some(error(UnterminatedContinuationLine, idx0)),
            };
        }
    }

    // Can be called at any time during tokenization. Should be called at every
    // character when consuming a multi-character token.
    //
    // Returns None if continuation is skipped without issue. Return Some(err)
    // if there was an issue in the continuation.
    fn skip_continuation(&mut self) -> Option<Error> {
        if let Some((idx0, '&')) = self.lookahead {
            self.bump();
            if let Some(Err(err)) = self.continuation(idx0) {
                return Some(err);
            }
        }

        None
    }

    fn internal_next(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '+')) => {
                    self.bump();
                    Some(Ok((idx0, Plus, idx0 + 1)))
                }
                Some((idx0, '-')) => {
                    self.bump();
                    Some(Ok((idx0, Minus, idx0 + 1)))
                }
                Some((idx0, '*')) => {
                    self.bump();

                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    match self.lookahead {
                        Some((idx1, '*')) => {
                            self.bump();
                            Some(Ok((idx0, StarStar, idx1 + 1)))
                        }
                        _ => Some(Ok((idx0, Star, idx0 + 1))),
                    }
                }
                Some((idx0, '/')) => {
                    self.bump();

                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    match self.lookahead {
                        Some((idx1, '/')) => {
                            self.bump();
                            Some(Ok((idx0, SlashSlash, idx1 + 1)))
                        }
                        _ => Some(Ok((idx0, Slash, idx0 + 1))),
                    }
                }
                Some((idx0, '(')) => {
                    self.bump();
                    Some(Ok((idx0, LeftParen, idx0 + 1)))
                }
                Some((idx0, ')')) => {
                    self.bump();
                    Some(Ok((idx0, RightParen, idx0 + 1)))
                }
                Some((idx0, '.')) => {
                    self.bump();
                    Some(self.operator(idx0))
                }
                Some((idx0, ';')) => {
                    self.bump();
                    Some(Ok((idx0, SemiColon, idx0 + 1)))
                }
                Some((idx0, ',')) => {
                    self.bump();
                    Some(Ok((idx0, Comma, idx0 + 1)))
                }
                Some((_, '&')) => {
                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    continue;
                }
                Some((idx0, c)) if (c == '"' || c == '\'') => {
                    self.bump();
                    Some(self.string_literal(idx0, c))
                }
                Some ((idx0, c)) if is_digit(c) => {
                    self.bump();
                    Some(self.digit_string(idx0))
                }
                // Handle LF
                Some((_, c)) if is_new_line_start(c) => self.consume_new_line(),
                Some((_, '!')) => {
                    self.bump();
                    self.commentary();
                    continue;
                }
                Some((idx0, c)) if is_identifier_start(c) => {
                    self.bump();
                    Some(self.identifierish(idx0))
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some((idx, _)) => Some(error(UnrecognizedToken, idx)),
                None => None,
            };
        }
    }

    fn take_until<F>(&mut self, idx0: usize, mut terminate: F) -> Option<Result<usize, Error>>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        let mut last_idx = idx0;
        loop {
            return match self.lookahead {
                Some((_, '&')) => {
                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    continue;
                }
                None => {
                    match terminate(EOF) {
                        Continue => {
                            debug_assert!(false);
                            Some(Ok(last_idx + 1))
                        }
                        Stop => Some(Ok(last_idx + 1)),
                        Error(err_code) => Some(error(err_code, idx0)),
                    }
                }
                Some((idx1, c)) => {
                    match terminate(Character(c)) {
                        Continue => {
                            self.bump();
                            last_idx = idx1;
                            continue;
                        }
                        Stop => Some(Ok(idx1)),
                        Error(err_code) => Some(error(err_code, idx0)),
                    }
                }
            };
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

fn is_new_line_start(c: char) -> bool {
    c == '\r' || c == '\n'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}