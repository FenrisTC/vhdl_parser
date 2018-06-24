extern crate vhdl_parser;

use vhdl_parser::token::*;
use vhdl_parser::lexer::*;
use vhdl_parser::parser::*;


#[test]
fn test_names() {

    let names = [
        "ieee.std_logic_1164.all",
        "attr'high",
        "push [ieee.std_logic return bit]",
        "vec(5)",
        "ifles'name(5)",
        "ifles'name.uea(5)",
        "push [ieee.std_logic return bit]'path",
    ];

    for &name in names.iter() {
        //println!("Testing String: {}", name);
        let mut ctx    : ParseContext = name.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_name = parser.parse_name();

        //println!("{:?}", ast_name);
        /*{
            let mut ctx = ParseContext::from(name);
            let mut lx : ScanInfo = (&mut ctx).into();
            loop {
                let tok = lx.scan_token();
                println!("Token: {:#?}", tok);
                if tok.kind == TokenKind::EoF { break; }
            }
        }*/
        assert!(ast_name.is_ok());

        let _ast_name = ast_name.unwrap();
        //println!("{:#?}", ast_name);
        println!("{:?}", parser.tok);
        //println!("length: {:?}", name.len());
        assert!(parser.tok.kind == TokenKind::EoF);
    }
}

//"vector_arr((others => '0'), (31 downto 0 => '1'), x\"x\")",
// Bug: There seems to be something wrong with the lexer 'x"x"' should be parsed
// as one string literal token but it seems to become two seperate ones.
#[test]
fn test_exprs() {
    let exprs = [
        "127",
        "(127)",
        "5 + 5",
        "(5 + 5)",
        "(5 * 3 + 5 ** 2)",
        "(a = 5 or (??b))",
        "rising_edge(clk) and o_vld = '1'",
        "(127 downto 96 => '1', others => '0')",
        "info.length + (4 - info.length(1 downto 0))",
        "vector_arr((others => '0'), (31 downto 0 => '1'), \"x\")",
        //"arr(indices range a to f)",
    ];

    for &expr in exprs.iter() {
        println!();
        println!("Testing Expr: {}", expr);

        let mut ctx : ParseContext = expr.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_expr = parser.parse_expression();
        //println!("Res: {:#?}", ast_expr);
        if ast_expr.is_err() {
            println!("Error: {:?}", ast_expr);
        }
        assert!(ast_expr.is_ok());

        let ast_expr = ast_expr.unwrap();
        println!("Res: {:#?}", ast_expr);

        assert!(parser.tok.kind == TokenKind::EoF);
    }
}

