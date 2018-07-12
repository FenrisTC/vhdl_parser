// AST Definiton
// Author: Sebastian Schüller <schueller@ti.uni-bonn.de>

use std;
use token::{Token, TokenKind, TokenKind::*};
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
            Abs      => Some(Op::Abs),
            Not      => Some(Op::Not),
            Star     => Some(Op::Mul),
            Slash    => Some(Op::Div),
            Mod      => Some(Op::Mod),
            Rem      => Some(Op::Rem),
            Plus     => Some(Op::Add),
            Minus    => Some(Op::Sub),
            Amp      => Some(Op::Cat),
            Sll      => Some(Op::Sll),
            Sla      => Some(Op::Srl),
            Sra      => Some(Op::Sla),
            Srl      => Some(Op::Sra),
            Rol      => Some(Op::Rol),
            Ror      => Some(Op::Ror),
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
            And      => Some(Op::And),
            Or       => Some(Op::Or),
            Xor      => Some(Op::Nand),
            Nand     => Some(Op::Nor),
            Nor      => Some(Op::Xor),
            Xnor     => Some(Op::Xnor),
            QQ       => Some(Op::QQ),
            _ => None,
        }
    }

    pub fn unary_from_token(t: &Token) -> Option<Op> {
        match t.kind {
            QQ    => Some(Op::QQ),
            Plus  => Some(Op::UnPos),
            Minus => Some(Op::UnNeg),
            And   => Some(Op::UnAnd),
            Or    => Some(Op::UnOr),
            Nand  => Some(Op::UnNand),
            Nor   => Some(Op::UnNor),
            Xor   => Some(Op::UnXnor),
            Xnor  => Some(Op::UnXnor),
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
pub enum Direction {
    To,
    Downto,
}

impl From<TokenKind> for Direction {
    fn from(k: TokenKind) -> Direction {
        match k {
            To     => Direction::To,
            Downto => Direction::Downto,
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
pub enum DiscreteRange {
    SubtypeIndication(SubtypeIndication),
    Range(RangeExpr),
    Attribute(Name),
}

impl DiscreteRange {
    pub fn try_from(expr: Expr) -> Option<DiscreteRange> {
        match expr {
            Expr {kind: ExprKind::Range(range), .. } => {
                let range   = DiscreteRange::Range(range);
                Some(range)
            },
            Expr {kind: ExprKind::SubtypeIndication(subtype), .. } => {
                let range   = DiscreteRange::SubtypeIndication(subtype);
                Some(range)
            },
            Expr {kind: ExprKind::Name(name), ..} => {
                if !name.is_attribute() { return None; }
                let range   = DiscreteRange::Attribute(name);
                Some(range)
            },
            _=> None,
        }
    }
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
    pub choices: Box<Expr>,
    pub designator: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Paren{ lvl: u32, expr: Box<Expr> },
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    Qualified(QualifiedExpr),
    TypeCast(TypeCastExpr),
    Name(Name),
    Range(RangeExpr),
    Assoc(AssocExpr),
    List(Vec<Expr>),
    Inertial(Box<Expr>),
    NumLit(NumericLit),
    PhyLit(PhysicalLit),
    StrLit(StringLit),
    ChrLit(CharLit),
    SubtypeIndication(SubtypeIndication),
    Other,
    Open,
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

impl From<Vec<Expr>> for Expr {
    fn from(mut val: Vec<Expr>) -> Expr {
        assert!(val.len() >= 1);
        if val.len() == 1 {
            return val.pop().unwrap();
        }
        return Expr::new(
            val.first().unwrap().pos.to(&val.last().unwrap().pos),
            ExprKind::List(val)
        );
    }
}

impl Expr {
    pub fn new(pos: SrcPos, kind: ExprKind) -> Expr {
        Expr {
            pos,
            kind,
        }
    }

    pub fn new_paren(pos: SrcPos, inner: Expr) -> Expr {
        Expr::new(pos, ExprKind::Paren {
            lvl: 1 + inner.nesting_lvl(),
            expr: Box::new(inner),
        })
    }

    pub fn is_valid_choices(&self) -> bool {
        match self.kind {
            ExprKind::List(ref vec) => {
                vec.iter().map(|e| e.is_valid_choices())
                    .fold(true, |acc, x| acc && x)
            },
            ExprKind::Range(_)  => true,
            ExprKind::Other     => true,
            ExprKind::NumLit(_) => true,
            _ => false,
        }
    }


    pub fn is_name(&self) -> bool {
        match self.kind {
            ExprKind::Name(_) => true,
            _ => false,
        }
    }

    pub fn is_paren(&self) -> bool {
        match self.kind {
            ExprKind::Paren { .. } => true,
            _ => false,
        }
    }

    pub fn unwrap_name(self) -> Name {
        match self.kind {
            ExprKind::Name(name) => name,
            _ => panic!(),
        }

    }

    pub fn without_parens(&self) -> &Expr {
        let mut paren_expr = self;
        loop {
            match paren_expr.kind {
                ExprKind::Paren{lvl: _, ref expr} => paren_expr = expr,
                _ => return paren_expr,
            }
        }
    }

    pub fn is_valid_formal(&self) -> bool {
        match self.without_parens().kind {
            ExprKind::Name(_) => true,
            _ => false,
        }
    }

    pub fn is_valid_actual(&self) -> bool {
        match self.kind {
            ExprKind::Range(_) |
            ExprKind::List(_)  |
            ExprKind::Other    |
            ExprKind::Assoc(_) => false,
            _ => true,
        }
    }

    pub fn nesting_lvl(&self) -> u32 {
        match self.without_parens().kind {
            ExprKind::Paren{lvl, ..} => lvl,
            _ => 0,
        }
    }
}






#[derive(Debug, Clone)]
pub enum SegmentKind {
    QualifiedExpr(Box<Expr>),
    UnparsedBlob(Vec<Token>),
    AttachedExpression(Box<Expr>),
    AttachedRange(Box<Range>),
    CharLiteral(char),
    Signature(Box<Signature>),
    Attribute,
    AllQualifier,
    OperatorSymbol(Op),
    Identifier,
}

impl PartialEq<Self> for SegmentKind {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl SegmentKind {
    pub fn unwrap_qualified_expr(self) -> Box<Expr> {
        match self {
            SegmentKind::QualifiedExpr(x) => x,
            _ => panic!("Unwrapped Wrong SegmentKind!"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NameSegment {
    pub pos: SrcPos,
    pub kind: SegmentKind,
}

#[derive(Debug, Clone, Default)]
pub struct Name {
    pub pos: SrcPos,
    pub segments: Vec<NameSegment>,
}

impl Name {
    pub fn add_segment(&mut self, seg: NameSegment) {
        self.pos = self.pos.to(&seg.pos);
        self.segments.push(seg);
    }

    pub fn is_qualifiend_expr(&self) -> bool {
        if self.segments.len() < 2 {
            return false;
        }
        match self.segments.last().map(|ref seg| &seg.kind) {
            Some(SegmentKind::QualifiedExpr(_)) => true,
            _ => false,
        }
    }

    pub fn is_attribute(&self) -> bool {
        for segment in self.segments.iter().rev() {
            match segment.kind {
                SegmentKind::AttachedExpression(_) => continue,
                SegmentKind::Attribute => return true,
                _ => return false,
            };
        }
        false
    }

    pub fn is_simple(&self) -> bool {
        self.segments.len() == 1 &&
            match self.segments.first().map(|ref seg| &seg.kind) {
                Some(SegmentKind::Identifier) => true,
                _ => false,
        }
    }

    pub fn is_selected(&self) -> bool {
        self.segments.iter().map(|s| s.kind == SegmentKind::Identifier).fold(true, |acc, v| acc && v)
    }

    //
    // Incomplete, Sure Future Bug: Array constraints might span multiple
    // parenthesis and as such cover potentially the last *n* name segments.
    // Currently we only convert the very last segment to a constraint.
    // Ideally, if we pop constraints, we should be left with a selected name
    // that make a valid typemark.
    //      - Sebastian, 28.06.18
    //
    pub fn pop_constraint(&mut self) -> Option<Constraint> {
        match self.segments.last() {
            Some(NameSegment {pos: _, kind: SegmentKind::AttachedExpression(_)}) => (),
            _ => return None,
        }

        if let Some(NameSegment {pos: _, kind: SegmentKind::AttachedExpression(expr)}) = self.segments.last() {
            // Try to convert the expression to a valid segment constraint

            let element = ElementConstraint::try_from(*expr.clone());
            element.map(|e| Constraint::Element { pos: e.pos(), constraint: Box::new(e) })

        } else {
            None
        }
    }

}






#[derive(Debug, Clone)]
pub struct NumericLit {
    pub pos: SrcPos,
}

#[derive(Debug, Clone)]
pub struct PhysicalLit {
    pub pos: SrcPos,
    pub lit: Box<NumericLit>,
    pub unit: Box<Name>,
}

#[derive(Debug, Clone)]
pub struct StringLit {
    pub pos: SrcPos,
}

#[derive(Debug, Clone)]
pub struct CharLit {
    pub pos: SrcPos,
}

#[derive(Debug, Clone, Default)]
pub struct Identifier {
    pub pos: SrcPos,
}

#[derive(Debug, Clone, Default)]
pub struct AbstractLiteral {
    pub pos: SrcPos,
}

#[derive(Debug, Clone, Default)]
pub struct EnumVariant {
    pub pos: SrcPos,
}




#[derive(Debug, Clone)]
pub struct UseClause {
    pub pos: SrcPos,
    pub uses: Vec<Name>,
}

#[derive(Debug, Clone, Default)]
pub struct Signature {
    pub pos: SrcPos,
    pub parameter_typenames: Vec<Name>,
    pub return_typename: Option<Name>,
}

#[derive(Debug, Clone, Default)]
pub struct SubtypeIndication {
    pub pos: SrcPos,
    pub typemark: Name,
    pub resolution: Option<ResolutionIndication>,
    pub constraint: Option<Box<Constraint>>,
}

#[derive(Debug, Clone)]
pub enum ResolutionIndication {
    Function(Box<Name>),
    ArrayIndication{
        pos: SrcPos,
        lvl: u32,
        resolution: Box<ResolutionIndication>
    },
    RecordIndication{
        pos: SrcPos,
        resolutions: Vec<(Name, ResolutionIndication)>
    },
}

impl ResolutionIndication {
    pub fn is_function(&self) -> bool {
        match self {
            ResolutionIndication::Function(_) => true,
            _ => false,
        }
    }

    pub fn pos(&self) -> SrcPos {
        match self {
            ResolutionIndication::Function(name) => name.pos.clone(),
            ResolutionIndication::ArrayIndication {pos, ..} => pos.clone(),
            ResolutionIndication::RecordIndication {pos, ..} => pos.clone(),
        }
    }

    pub fn lvl(&self) -> u32 {
        match self {
            ResolutionIndication::ArrayIndication{lvl, ..}  => lvl.clone(),
            _ => 0,
        }
    }

    pub fn try_from(expr: Expr) -> Option<ResolutionIndication> {
        match expr {
            Expr {kind: ExprKind::Name(name), .. } => {
                Some(ResolutionIndication::Function(Box::new(name)))
            },
            Expr {kind: ExprKind::Paren {lvl, expr}, pos } => {
                let res = ResolutionIndication::try_from(*expr);
                if res.is_none() { return None; }
                Some(ResolutionIndication::ArrayIndication {
                    pos,
                    lvl,
                    resolution: Box::new(res.unwrap()),
                })
            },
            _ => None,
        }
    }

    pub fn try_into_name(self) -> Option<Name> {
        match self {
            ResolutionIndication::Function(name) => Some(*name),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Range {
    Name(Name),
    Expr(RangeExpr),
}

impl Range {
    pub fn pos(&self) -> SrcPos {
        match self {
            Range::Name(name) => name.pos,
            Range::Expr(expr) => expr.lhs.pos.to(&expr.rhs.pos),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ElementConstraint {
    ArrayOpen{pos: SrcPos, next: Option<Box<ElementConstraint>>},
    Array    {pos: SrcPos, constraints: Vec<DiscreteRange>, next: Option<Box<ElementConstraint>>},
    Record   {pos: SrcPos, constraints: Vec< (Box<Name>, Box<ElementConstraint>) >},
}

impl ElementConstraint {
    pub fn pos(&self) -> SrcPos {
        match self {
            ElementConstraint::ArrayOpen{pos, ..} => pos.clone(),
            ElementConstraint::Array{pos, ..}     => pos.clone(),
            ElementConstraint::Record{pos, ..}    => pos.clone(),
        }
    }

    fn try_from(expr: Expr) -> Option<ElementConstraint> {
        if let Expr {pos, kind: ExprKind::Paren{lvl, expr}} = expr {
            if lvl > 1 { return None; }
            if let Some(range) = DiscreteRange::try_from(*expr.clone()) {
                let element = ElementConstraint::Array {
                    pos,
                    constraints: vec![range],
                    next: None,
                };
                return Some(element);
            }

            match *expr {
                Expr {kind: ExprKind::List(exprs), .. } => {
                    let mut constraints = Vec::<DiscreteRange>::new();
                    for expr in exprs.iter() {
                        let range = DiscreteRange::try_from(expr.clone());
                        if range.is_none() {
                            return None;
                        }
                        constraints.push(range.unwrap());
                    }
                    None
                },
                Expr {kind: ExprKind::Open, .. } => {
                    let element = ElementConstraint::ArrayOpen { pos, next: None };
                    Some(element)
                },
                _ => None,
            }
        } else {
            None
        }
    }

}



#[derive(Debug, Clone)]
pub enum Constraint {
    Range   {pos: SrcPos, constraint: Box<Range>},
    Element {pos: SrcPos, constraint: Box<ElementConstraint>},
}

impl Constraint {
    pub fn new_range(pos: SrcPos, range: Range) -> Constraint{
        Constraint::Range {
            pos: pos,
            constraint: Box::new(range),
        }
    }

}

#[derive(Debug, Clone)]
pub enum EntityDesignator {
    Ident(Identifier),
    Char(CharLit),
    Op(OperatorSymbol),
    All(SrcPos),
    Others(SrcPos),
}

#[derive(Debug, Clone)]
pub struct AttributeSpec {
    pub pos: SrcPos,
    pub designator: Identifier,
    pub specification: Vec<EntityDesignator>,
    pub class: EntityClass,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct AttributeDecl {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub typemark: Box<Name>,
}

#[derive(Debug, Clone)]
pub enum AttributeSpecOrDecl {
    Decl(AttributeDecl),
    Spec(AttributeSpec),
}


#[derive(Debug, Clone)]
pub struct SecondaryUnitDecl {
    pub ident: Identifier,
    pub factor: Option<AbstractLiteral>,
    pub unit: Identifier,
}

#[derive(Debug, Clone)]
pub struct PhysTypeDef {
    pub primary: Identifier,
    pub secondaries: Vec<SecondaryUnitDecl>,
}

#[derive(Debug, Clone)]
pub enum ArrayDef {
    Unbounded(Vec<Name>),
    Constraint(Vec<DiscreteRange>),
}

#[derive(Debug, Clone)]
pub struct ArrayTypeDef {
    pub def: ArrayDef,
    pub subtype: SubtypeIndication,
}

#[derive(Debug, Clone)]
pub enum TypeDef {
    Number(Box<Range>),
    Enumeration(Vec<EnumVariant>),
    Physical(Box<PhysTypeDef>),
    Array(Box<ArrayTypeDef>),
    Record(Vec<(Vec<Identifier>, Box<SubtypeIndication>)>),
    Access(Box<SubtypeIndication>),
    File(Box<Name>),
    // Incomplete: We need to be able to parse
    // Subprogram declarations & instantiation declarations,
    // Attribute declarations and use clauses before we can parse
    // protected declarations of any kind.
    // Protected,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub pos: SrcPos,
    pub typename: Identifier,
    pub def: TypeDef,
}

#[derive(Debug, Clone)]
pub struct SubtypeDecl {
    pub pos: SrcPos,
    pub typename: Identifier,
    pub subtype: Box<SubtypeIndication>,
}

#[derive(Debug, Clone)]
pub enum SignalList {
    Signals(Vec<Name>),
    Others,
    All,
}

#[derive(Debug, Clone)]
pub struct DisconnectSpec {
    pub pos: SrcPos,
    pub signal_list: SignalList,
    pub typemark: Box<Name>,
    pub time: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectDeclKind {
    Constant,
    Signal,
    File,
    Variable,
    Shared,
}

#[derive(Debug, Clone, Copy)]
pub enum SignalKind {
    Register,
    Bus,
}

#[derive(Debug, Clone)]
pub struct ObjectDecl {
    pub pos: SrcPos,
    pub kind: ObjectDeclKind,
    pub idents: Vec<Identifier>,
    pub subtype: Box<SubtypeIndication>,
    pub default: Option<Box<Expr>>,
    pub signal_kind: Option<SignalKind>,
    pub file_open_kind: Option<Box<Expr>>,
    pub file_open_name: Option<Box<Name>>,
}

#[derive(Debug, Clone)]
pub enum EntityClass {
    Entity,
    Architecture,
    Configuration,
    Procedure,
    Function,
    Package,
    Type,
    Subtype,
    Constant,
    Signal,
    Variable,
    Component,
    Literal,
    Label,
    Units,
    Group,
    File,
    Property,
    Sequence,
}

#[derive(Debug, Clone)]
pub enum EntityClassEntry {
    Boxed(EntityClass),
    Unboxed(EntityClass),
}

#[derive(Debug, Clone)]
pub struct GroupDecl {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub template: Box<Name>,
    pub constituents: Vec<Name>,
}

#[derive(Debug, Clone)]
pub struct GroupTemplateDecl {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub entries: Vec<EntityClassEntry>,
}

#[derive(Debug, Clone)]
pub enum GroupingDecl {
    Template(GroupTemplateDecl),
    Group(GroupDecl),
}

#[derive(Debug, Clone)]
pub enum AliasDesignator {
    Ident(Identifier),
    CharLit(CharLit),
    OpSymbol(OperatorSymbol),
}

#[derive(Debug, Clone)]
pub struct AliasDecl {
    pub pos: SrcPos,
    pub designator: AliasDesignator,
    pub subtype: Option<Box<SubtypeIndication>>,
    pub name: Box<Name>,
}

#[derive(Debug, Clone)]
pub struct ComponentDecl {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub generics: Option<Vec<GenericDeclaration>>,
    pub ports: Option<Vec<PortDeclaration>>,
}


#[derive(Debug, Clone)]
pub enum Declaration {
    Alias(AliasDecl),
    AttributeDecl(AttributeDecl),
    AttributeSpec(AttributeSpec),
    Configuration,
    Component(ComponentDecl),
    Disconnect(DisconnectSpec),
    GroupDecl(GroupDecl),
    GroupTemplateDecl(GroupTemplateDecl),
    Object(ObjectDecl),
    PackageBody(PackageBody),
    PackageDecl(PackageDecl),
    PackageInst(PackageInstDecl),
    PslClkDecl,
    PslPropDecl,
    PslSeqDecl,
    SubprogramBody(SubprogramBody),
    SubprogramDecl(SubprogramSpec),
    SubprogramInst(SubprogramInstDecl),
    Subtype(SubtypeDecl),
    Type(TypeDecl),
    UseClause(UseClause),
}

impl From<SubprogramDeclPart> for Declaration {
    fn from(t: SubprogramDeclPart) -> Declaration {
        match t {
            SubprogramDeclPart::Decl(d) => Declaration::SubprogramDecl(d),
            SubprogramDeclPart::Body(b) => Declaration::SubprogramBody(b),
            SubprogramDeclPart::Inst(i) => Declaration::SubprogramInst(i),
        }
    }
}

impl From<AttributeSpecOrDecl> for Declaration {
    fn from(t: AttributeSpecOrDecl) -> Declaration {
        match t {
            AttributeSpecOrDecl::Decl(d) => Declaration::AttributeDecl(d),
            AttributeSpecOrDecl::Spec(s) => Declaration::AttributeSpec(s),
        }
    }
}

impl From<GroupingDecl> for Declaration {
    fn from(t: GroupingDecl) -> Declaration {
        match t {
            GroupingDecl::Template(g) => Declaration::GroupTemplateDecl(g),
            GroupingDecl::Group(g)    => Declaration::GroupDecl(g),
        }
    }
}

impl From<PackagingDecl> for Declaration {
    fn from(t: PackagingDecl) -> Declaration {
        match t {
            PackagingDecl::Decl(d) => Declaration::PackageDecl(d),
            PackagingDecl::Body(b) => Declaration::PackageBody(b),
            PackagingDecl::Inst(i) => Declaration::PackageInst(i),
        }
    }
}

impl Declaration {
    pub fn is_valid_for_entity_decl(&self) -> bool {
        match self {
            Declaration::Configuration => false,
            _ => true,
        }
    }

    pub fn is_valid_for_package_body(&self) -> bool {
        match self {
            _ => true,
        }
    }

    pub fn is_valid_for_package_decl(&self) -> bool {
        match self {
            _ => true,
        }
    }
}


#[derive(Debug, Clone, Default)]
pub struct EntityDeclaration {
    pub pos: SrcPos,
    pub name: Identifier,
    pub generics: Vec<GenericDeclaration>,
    pub ports: Vec<PortDeclaration>,
    pub decl_items: Vec<Declaration>,
}

#[derive(Debug, Clone, Default)]
pub struct InterfaceConstantDeclaration {
    pub pos: SrcPos,
    pub idents: Vec<Identifier>,
    pub subtype: SubtypeIndication,
    pub default_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct OperatorSymbol {
    pub pos: SrcPos,
    pub op: Op,
}

#[derive(Debug, Clone)]
pub enum Designator {
    Identifier(Identifier),
    OperatorSymbol(OperatorSymbol),
}

#[derive(Debug, Clone)]
pub enum SubprogramKind {
    Procedure,
    Function{
        is_pure: bool,
        return_type: Box<Name>,
    },
}

#[derive(Debug, Clone)]
pub struct SubprogramSpec {
    pub pos: SrcPos,
    pub kind: SubprogramKind,
    pub designator: Designator,
    pub generics: Option<Vec<GenericDeclaration>>,
    pub generic_maps: Option<Vec<Expr>>,
    pub parameters: Vec<InterfaceObjectDeclaration>,
}

#[derive(Debug, Clone)]
pub struct SubprogramBody {
    pub pos: SrcPos,
    pub spec: Box<SubprogramSpec>,
    //pub decls: Vec<SubprogramDeclItem>,
    //pub stmts: Vec<SequentialStatement>,
}

#[derive(Debug, Clone)]
pub struct SubprogramInstDecl {
    pub pos: SrcPos,
    pub designator: Designator,
    pub name: Box<Name>,
    pub generic_maps: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub enum SubprogramDeclPart {
    Decl(SubprogramSpec),
    Body(SubprogramBody),
    Inst(SubprogramInstDecl),
}

#[derive(Debug, Clone)] 
pub struct PackageDecl {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub generics: Option<Vec<GenericDeclaration>>,
    pub generic_maps: Option<Vec<Expr>>,
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct PackageBody {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct PackageInstDecl {
    pub pos: SrcPos,
    pub ident: Identifier,
    pub name: Box<Name>,
    pub generic_maps: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum PackagingDecl {
    Decl(PackageDecl),
    Body(PackageBody),
    Inst(PackageInstDecl),
}

#[derive(Debug, Clone, Copy)]
pub enum InterfaceObjectClass {
    Constant,
    Signal,
    Variable,
    File,
}

impl InterfaceObjectClass {
    pub fn try_from_token(kind: TokenKind) -> Option<InterfaceObjectClass> {
        match kind {
            Constant => Some(InterfaceObjectClass::Constant),
            Signal   => Some(InterfaceObjectClass::Signal),
            Variable => Some(InterfaceObjectClass::Variable),
            File     => Some(InterfaceObjectClass::File),
            _ => None,
        }
    }
}


#[derive(Debug, Clone, Default)]
pub struct InterfaceObjectDeclaration {
    pub pos:          SrcPos,
    pub class:        Option<InterfaceObjectClass>,
    pub idents:       Vec<Identifier>,
    pub mode:         Option<Mode>,
    pub subtype:      SubtypeIndication,
    pub is_bus:       bool,
    pub default_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum InterfaceSubprogramDefault {
    Name(Box<Name>),
    Box(SrcPos),
}


#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDeclaration {
    pub pos: SrcPos,
    pub kind: SubprogramKind,
    pub designator: Designator,
    pub parameters: Vec<InterfaceObjectDeclaration>,
    pub default: Option<InterfaceSubprogramDefault>,
}

#[derive(Debug, Clone)]
pub struct GenericMapAspect {
    pub pos: SrcPos,
    pub elements: Vec<Expr>,
}


#[derive(Debug, Clone)]
pub enum InterfacePackageGenericMap {
    Map(GenericMapAspect), // Incomplete: Implement geneneric_map_aspect!
    Box(SrcPos),
    Default(SrcPos),
}

impl InterfacePackageGenericMap {
    pub fn pos(&self) -> SrcPos {
        match self {
            InterfacePackageGenericMap::Map(ref map) => map.pos.clone(),
            InterfacePackageGenericMap::Box(pos) => pos.clone(),
            InterfacePackageGenericMap::Default(pos) => pos.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterfacePackageDeclaration {
    pub pos: SrcPos,
    pub name: Identifier,
    pub referred_pkg: Box<Name>,
    pub map: InterfacePackageGenericMap,
}

#[derive(Debug, Clone)]
pub enum GenericDeclaration {
    Constant(InterfaceConstantDeclaration),
    Type(Identifier),
    Subprogram(InterfaceSubprogramDeclaration),
    Package(InterfacePackageDeclaration),
}

#[derive(Debug, Clone, Default)]
pub struct PortDeclaration {
    pub pos: SrcPos,
    pub idents: Vec<Identifier>,
    pub mode: Mode,
    pub subtype: SubtypeIndication,
    pub is_bus: bool,
    pub default_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Mode {
    In,
    Out,
    Inout,
    Buffer,
    Linkage
}

impl Default for Mode {
    fn default() -> Mode {
        Mode::In
    }
}

impl Mode {
    pub fn try_from_tokenkind(kind: TokenKind) -> Option<Mode> {
        match kind {
            TokenKind::In =>      Some(Mode::In),
            TokenKind::Out =>     Some(Mode::Out),
            TokenKind::Inout =>   Some(Mode::Inout),
            TokenKind::Buffer =>  Some(Mode::Buffer),
            TokenKind::Linkage => Some(Mode::Linkage),
            _ => None,
        }
    }
}
