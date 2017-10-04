use super::{Tok, ErrorCode, Error, Tokenizer, CaseInsensitiveUserStr, UserStr};
use super::Tok::*;

enum Expectation<'a> {
    ExpectTok(Tok<'a>),
    ExpectErr(ErrorCode),
}

use self::Expectation::*;

fn gen_test(input: &str, expected: Vec<(&str, Expectation)>) {
    // use $ to signal EOL because it can be replaced with a single space
    // for spans, and because it applies also to r#XXX# style strings:
    let input = input.replace("$", "\n");

    let tokenizer = Tokenizer::new(&input);
    let len = expected.len();
    for (token, (expected_span, expectation)) in tokenizer.zip(expected.into_iter()) {
        let expected_start = expected_span.find("~").unwrap();
        let expected_end = expected_span.rfind("~").unwrap() + 1;
        println!("token: {:?}", token);
        match expectation {
            ExpectTok(expected_tok) => {
                assert_eq!(Ok((expected_start, expected_tok, expected_end)), token);
            }
            ExpectErr(expected_ec) => {
                assert_eq!(
                    Err(Error {
                        location: expected_start,
                        code: expected_ec,
                    }),
                    token
                )
            }
        }
    }

    // The string should end either with a 
    let tokenizer = Tokenizer::new(&input);
    match tokenizer.skip(len).next() {
        a @ Some(_) =>
            assert_eq!(Some(Ok((0, EOS, 0))), a),
        x => assert_eq!(None, x),
    }
}

fn test(input: &str, expected: Vec<(&str, Tok)>) {
    let generic_expected = expected
        .into_iter()
        .map(|(span, tok)| (span, ExpectTok(tok)))
        .collect();
    gen_test(input, generic_expected);
}

fn test_err(input: &str, expected: (&str, ErrorCode)) {
    let (span, ec) = expected;
    gen_test(input, vec![(span, ExpectErr(ec))])
}

mod fortran_user_string {
    use super::{CaseInsensitiveUserStr, UserStr};

    #[test]
    fn basic() {
        assert_eq!(
            CaseInsensitiveUserStr::new("hello"),
            CaseInsensitiveUserStr::new("HELLO")
        );

        assert_eq!(
            CaseInsensitiveUserStr::new("hello"),
            CaseInsensitiveUserStr::new("Hello")
        );
    }

    #[test]
    fn split() {
        assert_eq!(
            UserStr::new("hello"),
            UserStr::new(
                r"h&
            &ello",
            )
        );

        assert_eq!(
            CaseInsensitiveUserStr::new("hello"),
            CaseInsensitiveUserStr::new(
                r"h&
            &eLLo",
            )
        );
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
mod tokenizer_tests {

use super::{test, test_err};
use super::{CaseInsensitiveUserStr, UserStr};
use super::Tok::*;
use super::ErrorCode::*;

#[test]
fn basic() {
    test("+ $", vec![
        ("~  ", Plus),
        ("  ~", EOS)
    ]);
}

#[test]
fn error() {
    test_err(".NO", 
            ("~  ", UnterminatedOperator)
    );
}

#[test]
fn operators() {
    test(".AND. .OR. .LT. .LE. .GT. .GE. .CUSTOM.", vec![
        ("~~~~~                                  ", And),
        ("      ~~~~                             ", Or),
        ("           ~~~~                        ", LessThan),
        ("                ~~~~                   ", LessThanOrEquals),
        ("                     ~~~~              ", GreaterThan),
        ("                          ~~~~         ", GreaterThanOrEquals),
        ("                               ~~~~~~~~", DefinedOperator(CaseInsensitiveUserStr::new("CUSTOM"))),
    ]);
}

#[test]
fn operators_lowercase() {
    test(".and. .or. .lt. .le. .gt. .ge. .custom.", vec![
        ("~~~~~                                  ", And),
        ("      ~~~~                             ", Or),
        ("           ~~~~                        ", LessThan),
        ("                ~~~~                   ", LessThanOrEquals),
        ("                     ~~~~              ", GreaterThan),
        ("                          ~~~~         ", GreaterThanOrEquals),
        ("                               ~~~~~~~~", DefinedOperator(CaseInsensitiveUserStr::new("CUSTOM"))),
    ]);
}

#[test]
fn operators_camelcase() {
    test(".And. .Or. .Lt. .Le. .Gt. .Ge. .Custom.", vec![
        ("~~~~~                                  ", And),
        ("      ~~~~                             ", Or),
        ("           ~~~~                        ", LessThan),
        ("                ~~~~                   ", LessThanOrEquals),
        ("                     ~~~~              ", GreaterThan),
        ("                          ~~~~         ", GreaterThanOrEquals),
        ("                               ~~~~~~~~", DefinedOperator(CaseInsensitiveUserStr::new("CUSTOM"))),
    ]);
}

#[test]
fn keywords() {
    test("PROGRAM END PRINT", vec![
        ("~~~~~~~          ", Program),
        ("        ~~~      ", End),
        ("            ~~~~~", Print),
    ]);
}

#[test]
fn keywords_lowercase() {
    test("program end print", vec![
        ("~~~~~~~          ", Program),
        ("        ~~~      ", End),
        ("            ~~~~~", Print),
    ]);
}

#[test]
fn keywords_camelcase() {
    test("Program End Print", vec![
        ("~~~~~~~          ", Program),
        ("        ~~~      ", End),
        ("            ~~~~~", Print),
    ]);
}

#[test]
fn logicals() {
    test(".TRUE. .FALSE.", vec![
        ("~~~~~~        ", True),
        ("       ~~~~~~~", False),
    ]);
}

#[test]
fn hello_world() {
    test(r#"PROGRAM hello; print *,"Hello, world!"; END PROGRAM hello"#, vec![
        (  "~~~~~~~                                                  ", Program),
        (  "        ~~~~~                                            ", Id(CaseInsensitiveUserStr::new("hello"))),
        (  "             ~                                           ", EOS),
        (  "               ~~~~~                                     ", Print),
        (  "                     ~                                   ", Star),
        (  "                      ~                                  ", Comma),
        (  "                       ~~~~~~~~~~~~~~~                   ", CharLiteralConstant(UserStr::new("Hello, world!"))),
        (  "                                      ~                  ", EOS),
        (  "                                        ~~~              ", End),
        (  "                                            ~~~~~~~      ", Program),
        (  "                                                    ~~~~~", Id(CaseInsensitiveUserStr::new("hello"))),
    ]);
}

#[test]
fn split_keywords() {
    test("&$pr&$    &og&$    &ram &$    hel&$    &lo", vec![
        ("  ~~~~~~~~~~~~~~~~~~~~~                   ", Program),
        ("                              ~~~~~~~~~~~~", Id(CaseInsensitiveUserStr::new("hello"))),
    ]);
}

}
