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

#[test]
fn test_generic_map() {
    let tests = [
        "generic map (complex_fixed_left =>
complex_math_fixed_left,
complex_fixed_right =>
complex_math_fixed_right,
complex_fixed_formal_pkg =>
complex_math_fixed_formal_pkg)
",
    ];


    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_interface_package_generic_map();
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
fn test_type_decl() {
    let tests = [
        // Integer point types
        "type A is range 1 to 10;",
        "type MULTI_LEVEL_LOGIC is (LOW, HIGH, RISING, FALLING, AMBIGUOUS);",
        "type BIT is ('0','1');",
        "type SWITCH_LEVEL is ('0','1','X');",
"type DURATION is range -1E18 to 1E18
units
    fs; --femtosecond
    ps = 1000 fs; --picosecond
    ns = 1000 ps; --nanosecond
    us = 1000 ns; --microsecond
    ms = 1000 us; --millisecond
    sec = 1000 ms; --second
    min = 60 sec; --minute
end units;",
"type DISTANCE is range 0 to 1E16
units
    -- primary unit:
    Å;
    -- metric lengths:
    nm = 10 Å;
    um = 1000 nm;
    mm = 1000 um;
    cm = 10 mm;
    m = 1000 mm;
    km = 1000 m;
    mil = 254000 Å;
    inch = 1000 mil;
    ft = 12 inch;
    yd = 3 ft;
    fm = 6 ft;
    mi = 5280 ft;
    lg = 3 mi;
end units DISTANCE;",
    "type MY_WORD is array (0 to 31) of BIT;",
    "type DATA_IN is array (7 downto 0) of FIVE_LEVEL_LOGIC;",
    "type MEMORY is array (INTEGER range <>) of MY_WORD;",
    "type SIGNED_FXPT_VECTOR is array (NATURAL range <>) of SIGNED_FXPT;",
    "type SIGNED_FXPT_5x4 is array (1 to 5, 1 to 4) of SIGNED_FXPT;",
    "type T is array (POSITIVE range MIN_BOUND to MAX_BOUND) of ELEMENT;",
    "type array_type is array (index_subtype range <>) of ELEMENT'BASE;",
    "type T is array (INTEGER range <>) of STRING(1 to 10);",
    "type array_type is array (INTEGER range <>) of STRING'BASE;",
"type DATE is
    record
        DAY : INTEGER range 1 to 31;
        MONTH : MONTH_NAME;
        YEAR : INTEGER range 0 to 4000;
    end record;",
    "type FT is file of TM;",
    "type ADDRESS is access MEMORY;",
    "type BUFFER_PTR is access TEMP_BUFFER;",
    ];


    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_type_decl();
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
fn test_object_declarations() {
    let tests = [
        "constant TOLER: DISTANCE := 1.5 nm;",
        "constant PI: REAL := 3.141592;",
        "constant CYCLE_TIME: TIME := 100 ns;",
        "constant Propagation_Delay: DELAY_LENGTH; -- A deferred constant.",
        "signal S: STANDARD.BIT_VECTOR (1 to 10);",
        "signal CLK1, CLK2: TIME;",
        "signal OUTPUT: WIRED_OR MULTI_VALUED_LOGIC;",
        "variable INDEX: INTEGER range 0 to 99 := 0;",
        "variable COUNT: POSITIVE;",
        "variable MEMORY: BIT_MATRIX (0 to 7, 0 to 1023);",
        "shared variable Counter: SharedCounter;",
        "shared variable addend, augend, result: ComplexNumber;",
        "variable bit_stack: VariableSizeBitArray;",
        "file F1: IntegerFile;",
        "file F2: IntegerFile is \"test.dat\";",
        "file F3: IntegerFile open WRITE_MODE is \"test.dat\";",
    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_object_decl();
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
fn test_attribute_decl_or_spec() {
    let tests = [
        "attribute LOCATION: COORDINATE;",
        "attribute PIN_NO: POSITIVE;",
        "attribute PIN_NO of CIN: signal is 10;",
        "attribute PIN_NO of COUT: signal is 5;",
        "attribute LOCATION of ADDER1: label is (10,15);",
        "attribute LOCATION of others: label is (25,77);",
        "attribute CAPACITANCE of all: signal is 15 pF;",
        "attribute IMPLEMENTATION of G1: group is \"74LS152\";",
        "attribute RISING_DELAY of C2Q: group is 7.2 ns;",
    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_attribute_decl_or_spec();
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
fn test_alias() {
    let tests = [
        "alias SIGN: BIT is REAL_NUMBER (0);",
        "alias MANTISSA: BIT_VECTOR (23 downto 0) is REAL_NUMBER (8 to 31);",
        "alias EXPONENT: BIT_VECTOR (1 to 7) is REAL_NUMBER (1 to 7);",
        "alias STD_BIT is STD.STANDARD.BIT;",
        "alias '0' is STD.STANDARD.'0' [return STD.STANDARD.BIT];",
        "alias '1' is STD.STANDARD.'1' [return STD.STANDARD.BIT];",
        "alias \"and\" is STD.STANDARD.\"and\" [STD.STANDARD.BIT, STD.STANDARD.BIT return STD.STANDARD.BIT];",
    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_alias_decl();
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
fn test_subprogram_part() {
    //
    // Incomplete: Also test subprogram bodies as soon as
    // we actually parse those.
    //
    let tests = [
        "function F return INTEGER;",
        "procedure Dump (F: inout Text; Value: Integer);",
        "procedure Check (Setup: Time; signal D: Data; signal C: Clock);",
        "function \"and\" (Left, Right: MVL) return MVL;",
        "function \"not\" (Value: MVL) return MVL;",
        "function \"xor\" (Right: MVL_Vector) return MVL;",
        "procedure swap generic ( type T ) parameter ( a, b : inout T );",
        "procedure int_swap is new swap generic map ( T => integer );",
        "procedure vec_swap is new swap generic map ( T => bit_vector(0 to 7) );",
        "procedure check_setup generic ( type signal_type; type clk_type; clk_active_value : clk_type; T_su : delay_length ) ( signal s : signal_type; signal clk : clk_type );",
    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_subprogram_decl_part();
        if !ast_test.is_ok() {
            println!("Err: {:?}", ast_test);
        }
        assert!(ast_test.is_ok());

        let ast_test = ast_test.unwrap();
        println!("Res: {:#?}", ast_test);

        assert!(parser.tok.kind == TokenKind::EoF, "{:?}", parser.tok);
    }
}

#[test]
fn test_packaging_decl() {
    let tests = [
"package TimeConstants is constant tPLH: Time := 10 ns;
    type Tri is ('0', '1', 'Z', 'E');
    function BitVal (Value: Tri) return Bit;
    function TriVal (Value: Bit) return Tri;
    type TriVector is array (Natural range <>) of Tri;
    function Resolve (Sources: TriVector) return Tri;
end package TriState;",

"package integer_integer_pkg is new work.generic_pkg
generic map ( T1 => integer, T2 => integer );",

"package body ID_manager is
    variable next_ID : natural := 0;
    impure function get_ID return natural;
end package body ID_manager;",

    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_packing_decl();
        if !ast_test.is_ok() {
            println!("Err: {:?}", ast_test);
        }
        assert!(ast_test.is_ok());

        let ast_test = ast_test.unwrap();
        println!("Res: {:#?}", ast_test);

        assert!(parser.tok.kind == TokenKind::EoF, "{:?}", parser.tok);
    }
}

#[test]
fn test_component_decl() {
    let tests = [
"component AND_GATE is
    generic (I1toO, I2toO: DELAY_LENGTH := 4 ns);
    port (I1, I2: in BIT; O: out BIT);
end component AND_GATE;"
    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_component_decl();
        if !ast_test.is_ok() {
            println!("Err: {:?}", ast_test);
        }
        assert!(ast_test.is_ok());

        let ast_test = ast_test.unwrap();
        println!("Res: {:#?}", ast_test);

        assert!(parser.tok.kind == TokenKind::EoF, "{:?}", parser.tok);
    }
}

#[test]
fn test_configuration() {
    let tests = [
"configuration V4_27_87 of Processor is
    use Work.all;
    for Structure_View
        for A1: ALU
            use configuration TTL.SN74LS181;
        end for;
        for M1,M2,M3: MUX
            use entity Multiplex4 (Behavior);
        end for;
        for all: Latch
            -- use defaults
        end for;
    end for;
end configuration V4_27_87;",

    ];
    for &test in tests.iter() {
        println!();
        println!("Testing: {}", test);

        let mut ctx : ParseContext = test.into();
        let mut parser : ParseInfo = (&mut ctx).into();
        let ast_test = parser.parse_configuration();
        if !ast_test.is_ok() {
            println!("Err: {:?}", ast_test);
        }
        assert!(ast_test.is_ok());

        let ast_test = ast_test.unwrap();
        println!("Res: {:#?}", ast_test);

        assert!(parser.tok.kind == TokenKind::EoF, "{:?}", parser.tok);
    }
}
