extern crate vhdl_parser;

use vhdl_parser::token::*;
use vhdl_parser::lexer::*;

#[test]
fn simple_test() {
    let test_str = "use ieee.std_logic_1164.all;";
    {
        let mut parse_ctx = ParseContext {
            txt: &test_str,
            line_offsets: Vec::default()
        };

        let mut lex_info = ScanInfo::from(&mut parse_ctx);
        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::Use,
            "{:?}, {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::Ident,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::Dot,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::Ident,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::Dot,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::All,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert_eq!(tok.kind, TokenKind::Semicolon,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.pos.as_range()]));

        let tok = lex_info.scan_token();
        assert!(tok.kind == TokenKind::EoF);
    }
}

#[test]
fn test_simple_file() {
    let test_str = include_str!("./test_data/hello.vhd");

    let mut parse_context = ParseContext::from(test_str);

    let mut lex_info = ScanInfo::from(&mut parse_context);

    let mut tokens: Vec<Token> = Vec::default();
    loop {
        let tok = lex_info.scan_token();
        tokens.push(tok.clone());
        if tok.kind == TokenKind::EoF || tok.kind == TokenKind::Invalid {
            break;
        }
    }

    println!("");
    for tok in tokens {
        println!("{:?} :: {:?}", tok.kind, tok);
    }
}
