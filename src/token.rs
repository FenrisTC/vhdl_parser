// Token definitions
// Author: Sebastian Schüller <schueller.ti.uni-bonn.de>

use SrcPos;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: SrcPos,
}

impl Token {
    pub fn invalid() -> Token {
        Token { kind: TokenKind::Invalid, pos: SrcPos(0,0), }
    }
}

impl Token {
    pub fn match_keyword(s: &str) -> Option<TokenKind> {
        match s.to_ascii_lowercase().as_ref() {
            "access"        => Some(TokenKind::Access),
            "after"         => Some(TokenKind::After),
            "alias"         => Some(TokenKind::Alias),
            "all"           => Some(TokenKind::All),
            "architecture"  => Some(TokenKind::Architecture),
            "array"         => Some(TokenKind::Array),
            "assert"        => Some(TokenKind::Assert),
            "attribute"     => Some(TokenKind::Attribute),
            "begin"         => Some(TokenKind::Begin),
            "block"         => Some(TokenKind::Block),
            "body"          => Some(TokenKind::Body),
            "buffer"        => Some(TokenKind::Buffer),
            "bus"           => Some(TokenKind::Bus),
            "case"          => Some(TokenKind::Case),
            "component"     => Some(TokenKind::Component),
            "configuration" => Some(TokenKind::Configuration),
            "constant"      => Some(TokenKind::Constant),
            "disconnect"    => Some(TokenKind::Disconnect),
            "downto"        => Some(TokenKind::Downto),
            "else"          => Some(TokenKind::Else),
            "elsif"         => Some(TokenKind::Elsif),
            "end"           => Some(TokenKind::End),
            "entity"        => Some(TokenKind::Entity),
            "exit"          => Some(TokenKind::Exit),
            "file"          => Some(TokenKind::File),
            "for"           => Some(TokenKind::For),
            "function"      => Some(TokenKind::Function),
            "generate"      => Some(TokenKind::Generate),
            "generic"       => Some(TokenKind::Generic),
            "guarded"       => Some(TokenKind::Guarded),
            "if"            => Some(TokenKind::If),
            "in"            => Some(TokenKind::In),
            "inout"         => Some(TokenKind::Inout),
            "is"            => Some(TokenKind::Is),
            "label"         => Some(TokenKind::Label),
            "library"       => Some(TokenKind::Library),
            "linkage"       => Some(TokenKind::Linkage),
            "loop"          => Some(TokenKind::Loop),
            "map"           => Some(TokenKind::Map),
            "new"           => Some(TokenKind::New),
            "next"          => Some(TokenKind::Next),
            "null"          => Some(TokenKind::Null),
            "of"            => Some(TokenKind::Of),
            "on"            => Some(TokenKind::On),
            "open"          => Some(TokenKind::Open),
            "others"        => Some(TokenKind::Others),
            "out"           => Some(TokenKind::Out),
            "package"       => Some(TokenKind::Package),
            "port"          => Some(TokenKind::Port),
            "procedure"     => Some(TokenKind::Procedure),
            "process"       => Some(TokenKind::Process),
            "range"         => Some(TokenKind::KwRange),
            "record"        => Some(TokenKind::Record),
            "register"      => Some(TokenKind::Register),
            "report"        => Some(TokenKind::Report),
            "return"        => Some(TokenKind::Return),
            "select"        => Some(TokenKind::Select),
            "severity"      => Some(TokenKind::Severity),
            "signal"        => Some(TokenKind::Signal),
            "subtype"       => Some(TokenKind::Subtype),
            "then"          => Some(TokenKind::Then),
            "to"            => Some(TokenKind::To),
            "transport"     => Some(TokenKind::Transport),
            "type"          => Some(TokenKind::Type),
            "units"         => Some(TokenKind::Units),
            "until"         => Some(TokenKind::Until),
            "use"           => Some(TokenKind::Use),
            "variable"      => Some(TokenKind::Variable),
            "wait"          => Some(TokenKind::Wait),
            "when"          => Some(TokenKind::When),
            "while"         => Some(TokenKind::While),
            "with"          => Some(TokenKind::With),
            "xnor"          => Some(TokenKind::Xnor),
            "group"         => Some(TokenKind::Group),
            "impure"        => Some(TokenKind::Impure),
            "inertial"      => Some(TokenKind::Inertial),
            "literal"       => Some(TokenKind::Literal),
            "postponed"     => Some(TokenKind::Postponed),
            "pure"          => Some(TokenKind::Pure),
            "reject"        => Some(TokenKind::Reject),
            "shared"        => Some(TokenKind::Shared),
            "unaffected"    => Some(TokenKind::Unaffected),
            "sll"           => Some(TokenKind::Sll),
            "sla"           => Some(TokenKind::Sla),
            "sra"           => Some(TokenKind::Sra),
            "srl"           => Some(TokenKind::Srl),
            "rol"           => Some(TokenKind::Rol),
            "ror"           => Some(TokenKind::Ror),
            "protected"     => Some(TokenKind::Protected),
            "context"       => Some(TokenKind::Context),
            "parameter"     => Some(TokenKind::Parameter),
            "mod"           => Some(TokenKind::Mod),
            "rem"           => Some(TokenKind::Rem),
            "and"           => Some(TokenKind::And),
            "or"            => Some(TokenKind::Or),
            "xor"           => Some(TokenKind::Xor),
            "nand"          => Some(TokenKind::Nand),
            "nor"           => Some(TokenKind::Nor),
            "abs"           => Some(TokenKind::Abs),
            "not"           => Some(TokenKind::Not),
            "vunit"         => Some(TokenKind::Vunit),
            _               => None
        }
    }

    /*
    pub fn match_operator(i: &str) -> Option<TokenKind> {
        match i.to_ascii_lowercase().as_ref() {
            "mod"  => Some(TokenKind::Mod),
            "rem"  => Some(TokenKind::Rem),
            "and"  => Some(TokenKind::And),
            "or"   => Some(TokenKind::Or),
            "xor"  => Some(TokenKind::Xor),
            "nand" => Some(TokenKind::Nand),
            "nor"  => Some(TokenKind::Nor),
            "abs"  => Some(TokenKind::Abs),
            "not"  => Some(TokenKind::Not),
            _      => None,
        }
    }
    */
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    NumDecLiteral,
    NumBaseLiteral,
    CharLiteral,
    StringLiteral,
    BitStringLiteral,
    Ident,

    LParen,        // (
    RParen,        // )
    LBracket,      // [
    RBracket,      // ]
    LBrace,        // {
    RBrace,        // }
    Colon,         // :
    Semicolon,     // ;
    Comma,         // ,
    Tick,          // '
    Bar,           // |
    LtGt,          // <>
    ColonEq,       // :=
    Dot,           // .
    At,            // @

    Eq,          // =
    SlashEq,     // /=
    Lt,          // <
    Gt,          // >
    EqGt,        // =>
    GEq,         // >=
    LEq,         // <=
    Plus,        // +
    Minus,       // -
    Amp,         // &
    QEq,         // ?=
    QSlashEq,    // ?/=
    QLt,         // ?<
    QLEq,        // ?<=
    QGt,         // ?>
    QGEq,        // ?>=
    Star,        // *
    StarStar,    // **
    Slash,       // /

    // VHDL 2008
    LtLt,          // <<
    GtGt,          // >>
    Caret,         // ^
    QQ,          // ??

    // Non-Semantic symbols
    Comment,
    Whitespace,
    Linebreak,
    EoF,
    Invalid,

    // Keywords
    Access,
    After,
    Alias,
    All,
    Architecture,
    Array,
    Assert,
    Attribute,
    Begin,
    Block,
    Body,
    Buffer,
    Bus,
    Case,
    Component,
    Configuration,
    Constant,
    KwDefault,
    Disconnect,
    Downto,
    Else,
    Elsif,
    End,
    Entity,
    Exit,
    File,
    For,
    Function,
    Generate,
    Generic,
    Guarded,
    If,
    In,
    Inout,
    Is,
    Label,
    Library,
    Linkage,
    Loop,
    Map,
    New,
    Next,
    Null,
    Of,
    On,
    Open,
    Others,
    Out,
    Package,
    Port,
    Procedure,
    Process,
    Property,
    KwRange,
    Record,
    Register,
    Report,
    Return,
    Select,
    Severity,
    Sequence,
    Signal,
    Subtype,
    Then,
    To,
    Transport,
    Type,
    Units,
    Until,
    Use,
    Variable,
    Wait,
    When,
    While,
    With,

    Mod,
    Rem,
    And,
    Or,
    Xor,
    Nand,
    Nor,
    Abs,
    Not,

    // VHDL 1993
    Xnor,
    Group,
    Impure,
    Inertial,
    Literal,
    Postponed,
    Pure,
    Reject,
    Shared,
    Unaffected,
    Sll,
    Sla,
    Sra,
    Srl,
    Rol,
    Ror,

    // VHDL 2000
    Protected,

    // VHDL 2008
    Context,
    Parameter,
    Vunit,
}
