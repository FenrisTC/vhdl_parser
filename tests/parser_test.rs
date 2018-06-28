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
        println!();
        println!("Testing Name: {}", name);
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
        "arr(indices range a to f)",
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

#[test]
fn test_subtype_indications() {
    let tests = [
        "std_logic_vector",
        "std_logic_vector(0 to 31)",
        "std_logic_vector(data_bits - 1 downto 0)",
        "natural range 0 to natural'high",
        "array_sig range indices'range",
        "arr(indices range a to g)",
        "resolved std_ulogic",
        "resolved std_ulogic_vector(5 downto 0)",
        "(resolved) std_ulogic_vector",
        "(vld ored, req anded, data xored) struct_type",
    ];


    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_subtype_indication();
        if !ast_test.is_ok() {
            println!("Err: {:?}", ast_test);
        }
        assert!(ast_test.is_ok());

        let ast_test = ast_test.unwrap();
        println!("Res: {:#?}", ast_test);

        assert!(parser.tok.kind == TokenKind::EoF);
    }
}

#[test]
fn test_entity_declarations() {
    let tests = [
"\
entity e is
end entity;
",
"\
entity test is
generic(data_bits: natural := 8);
port(i,c,b: bit; o: out bit_vector(31 downto 0));
end entity;\
",
"\
entity a is
generic(type data_t; address: natural; procedure set_alarm(signal is_alarm: inout bit) is <>);
end entity a;
",
"\
entity weird is
port(o: out std_logic_vector(31 downto 0) := std_logic_vector(
        7 => 'x',
        6 => '0',
        5 downto 0 => 'w',
        others => '-'));
end entity;
",

    ];

    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_entity_decl();
        if !ast_test.is_ok() {
            println!("Err: {:?}", ast_test);
        }
        assert!(ast_test.is_ok());

        let ast_test = ast_test.unwrap();
        println!("Res: {:#?}", ast_test);

        assert!(parser.tok.kind == TokenKind::EoF);
    }
}
