// Token definitions
// Author: Sebastian Schüller <schueller.ti.uni-bonn.de>


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub str_range: (usize, usize),
    pub file_pos:  (usize, usize),
}

impl Token {
    pub fn match_keyword(s: &str) -> Option<Keyword> {
        match s.to_ascii_lowercase().as_ref() {
            "access"        => Some(Keyword::Access),
            "after"         => Some(Keyword::After),
            "alias"         => Some(Keyword::Alias),
            "all"           => Some(Keyword::All),
            "architecture"  => Some(Keyword::Architecture),
            "array"         => Some(Keyword::Array),
            "assert"        => Some(Keyword::Assert),
            "attribute"     => Some(Keyword::Attribute),
            "begin"         => Some(Keyword::Begin),
            "block"         => Some(Keyword::Block),
            "body"          => Some(Keyword::Body),
            "buffer"        => Some(Keyword::Buffer),
            "bus"           => Some(Keyword::Bus),
            "case"          => Some(Keyword::Case),
            "component"     => Some(Keyword::Component),
            "configuration" => Some(Keyword::Configuration),
            "constant"      => Some(Keyword::Constant),
            "disconnect"    => Some(Keyword::Disconnect),
            "downto"        => Some(Keyword::Downto),
            "else"          => Some(Keyword::Else),
            "elsif"         => Some(Keyword::Elsif),
            "end"           => Some(Keyword::End),
            "entity"        => Some(Keyword::Entity),
            "exit"          => Some(Keyword::Exit),
            "file"          => Some(Keyword::File),
            "for"           => Some(Keyword::For),
            "function"      => Some(Keyword::Function),
            "generate"      => Some(Keyword::Generate),
            "generic"       => Some(Keyword::Generic),
            "guarded"       => Some(Keyword::Guarded),
            "if"            => Some(Keyword::If),
            "in"            => Some(Keyword::In),
            "inout"         => Some(Keyword::Inout),
            "is"            => Some(Keyword::Is),
            "label"         => Some(Keyword::Label),
            "library"       => Some(Keyword::Library),
            "linkage"       => Some(Keyword::Linkage),
            "loop"          => Some(Keyword::Loop),
            "map"           => Some(Keyword::Map),
            "new"           => Some(Keyword::New),
            "next"          => Some(Keyword::Next),
            "null"          => Some(Keyword::Null),
            "of"            => Some(Keyword::Of),
            "on"            => Some(Keyword::On),
            "open"          => Some(Keyword::Open),
            "others"        => Some(Keyword::Others),
            "out"           => Some(Keyword::Out),
            "package"       => Some(Keyword::Package),
            "port"          => Some(Keyword::Port),
            "procedure"     => Some(Keyword::Procedure),
            "process"       => Some(Keyword::Process),
            "range"         => Some(Keyword::Range),
            "record"        => Some(Keyword::Record),
            "register"      => Some(Keyword::Register),
            "report"        => Some(Keyword::Report),
            "return"        => Some(Keyword::Return),
            "select"        => Some(Keyword::Select),
            "severity"      => Some(Keyword::Severity),
            "signal"        => Some(Keyword::Signal),
            "subtype"       => Some(Keyword::Subtype),
            "then"          => Some(Keyword::Then),
            "to"            => Some(Keyword::To),
            "transport"     => Some(Keyword::Transport),
            "type"          => Some(Keyword::Type),
            "units"         => Some(Keyword::Units),
            "until"         => Some(Keyword::Until),
            "use"           => Some(Keyword::Use),
            "variable"      => Some(Keyword::Variable),
            "wait"          => Some(Keyword::Wait),
            "when"          => Some(Keyword::When),
            "while"         => Some(Keyword::While),
            "with"          => Some(Keyword::With),
            "xnor"          => Some(Keyword::Xnor),
            "group"         => Some(Keyword::Group),
            "impure"        => Some(Keyword::Impure),
            "inertial"      => Some(Keyword::Inertial),
            "literal"       => Some(Keyword::Literal),
            "postponed"     => Some(Keyword::Postponed),
            "pure"          => Some(Keyword::Pure),
            "reject"        => Some(Keyword::Reject),
            "shared"        => Some(Keyword::Shared),
            "unaffected"    => Some(Keyword::Unaffected),
            "sll"           => Some(Keyword::Sll),
            "sla"           => Some(Keyword::Sla),
            "sra"           => Some(Keyword::Sra),
            "srl"           => Some(Keyword::Srl),
            "rol"           => Some(Keyword::Rol),
            "ror"           => Some(Keyword::Ror),
            "protected"     => Some(Keyword::Protected),
            "context"       => Some(Keyword::Context),
            "parameter"     => Some(Keyword::Parameter),
            _               => None
        }
    }

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
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Kw(Keyword),

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
    Mod,         // mod
    Rem,         // rem
    And,         // and
    Or,          // or
    Xor,         // xor
    Nand,        // nand
    Nor,         // nor
    Abs,         // abs
    Not,         // not

    // VHDL 2008
    LtLt,          // <<
    GtGt,          // >>
    Caret,         // ^
    OpQQ,          // ??

    // Non-Semantic symbols
    Comment,
    Whitespace,
    Linebreak,
    EoF,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
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
    Range,
    Record,
    Register,
    Report,
    Return,
    Select,
    Severity,
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
}
