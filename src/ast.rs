use std;
use token::{Token, TokenKind, TokenKind::*, Keyword::*};
use SrcPos;

#[derive(Debug, Clone, Default)]
pub struct NodeId(u32);


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Exp,  // **
    Abs,  // abs
    Not,  // not
    Mul,  // *
    Div,  // /
    Mod,  // mod
    Rem,  // rem
    Add,  // +
    Sub,  // -
    Cat,  // & (concatenation)
    Sll,  // sll
    Srl,  // srl
    Sla,  // sla
    Sra,  // sra
    Rol,  // rol
    Ror,  // ror
    Eq,   // =
    NEq,  // /=
    Gt,   // >
    GEq,  // >=
    Lt,   // <
    LEq,  // <=
    QEq,  // ?=
    QNEq, // ?/=
    QLt,  // ?<
    QLEq, // ?<=
    QGt,  // ?>
    QGEq, // ?>=
    And,  // and
    Or,   // or
    Nand, // nand
    Nor,  // nor
    Xor,  // xor
    Xnor, // xnor
    QQ,   // ??
    UnPos,// + <>
    UnNeg,// - <>
    UnAnd,//
    UnOr, //
    UnNand,//
    UnNor,//
    UnXor,//
    UnXnor,
}


impl Op {
    pub fn from_op_symbol(s: &str) -> Option<Op> {
        match s {
            "abs"  => Some(Op::Abs),
            "not"  => Some(Op::Not),
            "mod"  => Some(Op::Mod),
            "rem"  => Some(Op::Rem),
            "sll"  => Some(Op::Sll),
            "srl"  => Some(Op::Srl),
            "sla"  => Some(Op::Sla),
            "sra"  => Some(Op::Sra),
            "rol"  => Some(Op::Rol),
            "ror"  => Some(Op::Ror),
            "and"  => Some(Op::And),
            "or"   => Some(Op::Or),
            "nand" => Some(Op::Nand),
            "nor"  => Some(Op::Nor),
            "xor"  => Some(Op::Xor),
            "xnor" => Some(Op::Xnor),
            "="    => Some(Op::Eq),
            "/="   => Some(Op::NEq),
            "<"    => Some(Op::Lt),
            ">"    => Some(Op::Gt),
            ">="   => Some(Op::GEq),
            "<="   => Some(Op::LEq),
            "+"    => Some(Op::Add),
            "-"    => Some(Op::Sub),
            "&"    => Some(Op::Cat),
            "?="   => Some(Op::QEq),
            "?/="  => Some(Op::QNEq),
            "?<"   => Some(Op::QLt),
            "?<="  => Some(Op::QLEq),
            "?>"   => Some(Op::QGt),
            "?>="  => Some(Op::QGEq),
            "*"    => Some(Op::Mul),
            "**"   => Some(Op::Exp),
            "/"    => Some(Op::Div),
            _      => None,
        }
    }

    // If a token can represent both a binary and unary
    // op, we default here to get the binary op.
    pub fn from_token(t: &Token) -> Option<Op> {
        match t.kind {
            StarStar => Some(Op::Exp),
            Kw(Abs)  => Some(Op::Abs),
            Kw(Not)  => Some(Op::Not),
            Star     => Some(Op::Mul),
            Slash    => Some(Op::Div),
            Kw(Mod)  => Some(Op::Mod),
            Kw(Rem)  => Some(Op::Rem),
            Plus     => Some(Op::Add),
            Minus    => Some(Op::Sub),
            Amp      => Some(Op::Cat),
            Kw(Sll)  => Some(Op::Sll),
            Kw(Sla)  => Some(Op::Srl),
            Kw(Sra)  => Some(Op::Sla),
            Kw(Srl)  => Some(Op::Sra),
            Kw(Rol)  => Some(Op::Rol),
            Kw(Ror)  => Some(Op::Ror),
            Eq       => Some(Op::Eq),
            SlashEq  => Some(Op::NEq),
            Gt       => Some(Op::Gt),
            GEq      => Some(Op::GEq),
            Lt       => Some(Op::Lt),
            LEq      => Some(Op::LEq),
            QEq      => Some(Op::QEq),
            QSlashEq => Some(Op::QNEq),
            QLt      => Some(Op::QLt),
            QLEq     => Some(Op::QLEq),
            QGt      => Some(Op::QGt),
            QGEq     => Some(Op::QGEq),
            Kw(And)  => Some(Op::And),
            Kw(Or)   => Some(Op::Or),
            Kw(Xor)  => Some(Op::Nand),
            Kw(Nand) => Some(Op::Nor),
            Kw(Nor)  => Some(Op::Xor),
            Kw(Xnor) => Some(Op::Xnor),
            QQ       => Some(Op::QQ),
            _ => None,
        }
    }

    pub fn unary_from_token(t: &Token) -> Option<Op> {
        match t.kind {
            Plus     => Some(Op::UnPos),
            Minus    => Some(Op::UnNeg),
            Kw(And)  => Some(Op::UnAnd),
            Kw(Or)   => Some(Op::UnOr),
            Kw(Nand) => Some(Op::UnNand),
            Kw(Nor)  => Some(Op::UnNor),
            Kw(Xor)  => Some(Op::UnXnor),
            Kw(Xnor) => Some(Op::UnXnor),
            _ => None,
        }

    }

    pub fn precedence(&self) -> u32 {
        match self {
            Op::Exp | Op::Abs  | Op::Not              => 7,
            Op::Mul | Op::Div  | Op::Mod | Op::Rem    => 6,
            Op::UnPos | Op::UnNeg                     => 5,
            Op::Add | Op::Sub  | Op::Cat              => 4,
            Op::Sll | Op::Srl  | Op::Sla |
            Op::Sra | Op::Rol  | Op::Ror              => 3,
            Op::Eq  | Op::NEq  | Op::Gt   |
            Op::GEq | Op::Lt   | Op::LEq  |
            Op::QEq | Op::QNEq | Op::QLt  | Op::QLEq |
            Op::QGt | Op::QGEq                        => 2,
            Op::UnAnd | Op::UnOr | Op::UnXor | Op::UnNand | Op::UnNor | Op::UnXnor |
            Op::And  | Op::Or   | Op::Nand| Op::Nor  |
            Op::Xor  | Op::Xnor                       => 1,
            Op::QQ                                    => 0,
        }
    }

    pub fn assoc(&self) -> Assoc {
        if *self == Op::Exp {
            return Assoc::Right;
        }
        return Assoc::Left;
    }

}

#[derive(Debug, Clone)]
pub struct NumericLiteral {
}


#[derive(Debug, Clone)]
pub enum Direction {
    To,
    Downto,
}

impl From<TokenKind> for Direction {
    fn from(k: TokenKind) -> Direction {
        match k {
            Kw(To)     => Direction::To,
            Kw(Downto) => Direction::Downto,
            _ => {
                panic!("Internal Compiler Error {}, {}: Tried to build a range direction from token other than To or Downto (namely this: {:?})", file!(), line!(), k);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub lhs: Box<Expr>,
    pub dir: Direction,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub lhs: Box<Expr>,
    pub op: Op,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnOpExpr {
    pub op: Op,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct QualifiedExpr {
    pub qualifier: Name,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeCastExpr {
    pub target_type: Name,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct AssocExpr {
    pub choices: Vec<Expr>,
    pub designator: Box<Expr>,
}


#[derive(Debug, Clone)]
pub enum ExprKind {
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    Qualified(QualifiedExpr),
    TypeCast(TypeCastExpr),
    Name(Name),
    NumericLiteral(NumericLiteral),
    Range(RangeExpr),
    Assoc(AssocExpr),
    Aggregate(Vec<Expr>),
    Other,
}

impl ExprKind {
    pub fn new_unop(op: Op, rhs: Expr) -> ExprKind {
        ExprKind::UnOp(UnOpExpr {
            op,
            rhs: Box::new(rhs),
        })
    }
}

impl PartialEq<Self> for ExprKind {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub pos: SrcPos,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(pos: SrcPos, kind: ExprKind) -> Expr {
        Expr {
            pos,
            kind,
        }
    }
}


#[derive(Debug, Clone, Default)]
pub struct Signature {
    pub pos: SrcPos,
    pub parameter_typenames: Vec<Name>,
    pub return_typename: Option<Name>,
}

#[derive(Debug, Clone)]
pub enum SegmentKind {
    QualifiedExpr(Expr),
    UnparsedBlob(Vec<Token>),
    CharLiteral(char),
    Signature(Signature),
    Attribute,
    AllQualifier,
    OperatorSymbol(Op),
    Identifier,
}

#[derive(Debug, Clone)]
pub struct NameSegment {
    pub pos: SrcPos,
    pub kind: SegmentKind,
}

#[derive(Debug, Clone, Default)]
pub struct Name {
    pub pos: SrcPos,
    pub id: NodeId,
    pub segments: Vec<NameSegment>,
}


