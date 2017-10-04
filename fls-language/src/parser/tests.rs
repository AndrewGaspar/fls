use super::fortran;
use super::super::tok;

#[test]
fn simplest() {
    let program = include_str!("programs/simplest.f90");

    let tokenizer = tok::Tokenizer::new(program);

    let program = fortran::parse_Program(tokenizer).unwrap();

    assert_eq!(1, program.units.len());
}

#[test]
fn hello_world() {
    let program = include_str!("programs/hello.f90");

    let tokenizer = tok::Tokenizer::new(program);

    let program = fortran::parse_Program(tokenizer).unwrap();

    assert_eq!(1, program.units.len());

    let ref program = &program.units[0];
    assert_eq!("hello", program.begin_name.as_ref().unwrap());
}