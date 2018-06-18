extern crate vhdl_parser;

use vhdl_parser::token::*;
use vhdl_parser::lexer::*;
use vhdl_parser::parser::*;


#[test]
fn test_names() {

    //let test_name = "ieee.std_logic_1164.all";
    //let test_name = "attr'high";
    let test_name = "push [ieee.std_logic return bit]";

    let mut ctx    : ParseContext = test_name.into();
    let mut parser : ParseInfo = (&mut ctx).into();
    let name = parser.parse_name();

    println!("{:?}", name);
    assert!(name.is_ok());

    let name = name.unwrap();
    println!("{:#?}", name);
    println!("{:?}", parser.tok);
    println!("length: {:?}", test_name.len());

}

