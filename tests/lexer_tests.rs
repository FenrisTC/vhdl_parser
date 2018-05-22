extern crate vhdl_parser_2;

use vhdl_parser_2::token::*;
use vhdl_parser_2::lexer::*;

#[test]
fn simple_test() {
    let test_str = "use ieee.std_logic_1164.all;";
    {
        let mut src_info = SrcInfo {
            txt: &test_str,
            line_offsets: Vec::default()
        };

        let mut lex_info = ScanInfo::from_src_info(&mut src_info);
        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Kw(Keyword::Use),
            "{:?}, {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Ident,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Dot,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Ident,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Dot,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Kw(Keyword::All),
            "{:?}, : {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token().unwrap();
        assert_eq!(tok.kind, TokenKind::Semicolon,
            "{:?}, : {:?}", tok, String::from(&test_str[tok.str_range.0..tok.str_range.1]));

        let tok = lex_info.scan_token();
        assert!(tok.is_none());
    }
}

#[test]
fn test_simple_file() {
    let test_str = include_str!("./test_data/hello.vhd");

    let mut src_info = SrcInfo::from_str(&test_str);

    let mut lex_info = ScanInfo::from_src_info(&mut src_info);

    let mut tokens: Vec<Token> = Vec::default();
    while let Some(tok) = lex_info.scan_token() {
        tokens.push(tok);
    }

    println!("");
    for tok in tokens {
        println!("{:?} :: {:?}", tok.kind, tok);
    }
}
