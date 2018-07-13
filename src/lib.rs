//#[macro_use]
//extern crate bitflags;

pub mod token;
pub mod lexer;
pub mod parser;
pub mod ast;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct SrcPos(pub u32, pub u32);

impl SrcPos {
    pub fn from_src_pos(pos: &SrcPos) -> SrcPos {
        pos.clone()
    }

    pub fn invalid() -> SrcPos {
        SrcPos(0,0)
    }

    pub fn to(&self, pos: &SrcPos) -> SrcPos {
        SrcPos(self.0, pos.1)
    }

    pub fn as_range(&self) -> std::ops::Range<usize> {
        std::ops::Range { start: self.0 as usize, end: self.1 as usize }
    }
}


#[derive(Debug, Clone)]
pub enum ParseError {
    ExprChoicesWithoutDesignator,
    InvalidOpSymbolString,
    InvalidDeclarationForEntity,
    InvalidDeclarationForPackageBody,
    InvalidDeclarationForPackageDecl,
    InvalidDeclarationForConfigurationDecl,
    MalformedExpr,
    MalformedName,
    MalformedDiscreteRange,
    MalformedGenericMapActual,
    MalformedGenericMapFormal,
    MalformedArrayDefinition,
    MixedArrayDefinition,
    SignalKindInNonSignalDecl,
    StringIsNotAnOpSymbol,
    NoReturnInFunction,
    ReturnInProcedure,
    PurityInProcedure,
    UnexpectedToken(token::Token, String, Vec<token::TokenKind>),
    UnexpectedEoF,
    Internal,
}

pub type PResult<T>=Result<T, ParseError>;
