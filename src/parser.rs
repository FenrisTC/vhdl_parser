
use token::*;
use token::Keyword::*;
use token::TokenKind::*;
use lexer::{ScanInfo, ParseContext};
use SrcPos;
use {ParseError, PResult};
use ast::*;


#[derive(Debug)]
pub struct ParseInfo<'a> {
    pub scan: ScanInfo<'a>,
    pub tok: Token,
    pub last_pos: SrcPos,
    pub expected: Vec<TokenKind>,
    pub errors: Vec<ParseError>,
    //pub cached_tokens: Vec<Token>,
    //pub tok_cursor: usize,
}

impl<'a> From<ScanInfo<'a>> for ParseInfo<'a> {
    fn from(scan: ScanInfo<'a>) -> ParseInfo<'a> {
        let mut parser = ParseInfo {
            scan: scan,
            tok: Token::invalid(),
            last_pos: SrcPos::default(),
            expected: Vec::default(),
            errors: Vec::default(),
        };
        parser.tok = parser.scan.scan_token();
        parser
    }
}

impl<'a> From<&'a mut ParseContext<'a>> for ParseInfo<'a> {
    fn from(ctx: &'a mut ParseContext<'a>) -> ParseInfo<'a> {
        let scan : ScanInfo = ctx.into();
        let mut parser = ParseInfo {
            scan: scan,
            tok: Token::invalid(),
            last_pos: SrcPos::default(),
            expected: Vec::default(),
            errors: Vec::default(),
        };
        parser.tok = parser.scan.scan_token();
        parser
    }

}

impl<'a> ParseInfo<'a> {
    fn unexpected_tok<T>(&mut self) -> PResult<T> {
        let err = ParseError::UnexpectedToken;
        self.errors.push(err);
        Err(err)
    }

    fn unexpected_eof<T>(&mut self) -> PResult<T> {
        let err = ParseError::UnexpectedEoF;
        self.errors.push(err);
        Err(err)
    }

    fn invalid_op_symbol<T>(&mut self) -> PResult<T> {
        let err = ParseError::InvalidOpSymbolString;
        self.errors.push(err);
        Err(err)
    }

    fn expr_choices_without_designator<T>(&mut self) -> PResult<T> {
        let err = ParseError::ExprChoicesWithoutDesignator;
        self.errors.push(err);
        Err(err)
    }

}

impl<'a> ParseInfo<'a> {
    fn kind(&self) -> TokenKind {
        self.tok.kind
    }

    fn pos(&self) -> SrcPos {
        self.tok.pos
    }

    fn tok_is(&mut self, k: TokenKind) -> bool {
        self.expected.push(k);
        self.kind() == k
    }

    fn tok_is_one_of(&mut self, kinds: &[TokenKind]) -> bool {
        kinds.contains(&self.kind())
    }
}

impl<'srcfile> ParseInfo<'srcfile> {
    fn advance_tok(&mut self) {
        debug_assert!(self.tok.kind != EoF);
        self.expected.clear();

        self.last_pos = self.tok.pos;
        let tok = self.scan.scan_token();
        self.tok = tok;
    }

    #[allow(dead_code)]
    fn parse_expr_atom(&mut self) -> PResult<Expr> {
        self.expected.extend_from_slice(&[
            Plus, Minus, Kw(And), Kw(Or), Kw(Xor), Kw(Not), Kw(Nand),
            Kw(Nor), Kw(Xnor)
        ]);
        let start = self.pos();
        if let Some(op) = Op::unary_from_token(&self.tok) {

            let rhs = self.parse_expr_with_precedence(op.precedence())?;
            let unop = ExprKind::new_unop(op, rhs);
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, unop));
        }

        if self.tok_is(Kw(Others)) {
            self.advance_tok();
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, ExprKind::Other));
        }

        if self.tok_is(Ident) {
            let name = self.parse_name()?;
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, ExprKind::Name(name)));
        }


        if self.tok_is(LParen) {
            self.advance_tok(); // Eat (
            let mut aggregates =  Vec::<Expr>::default();
            'aggregate_exprs: loop {
                let mut choices = Vec::<Expr>::default();
                let mut expr = self.parse_expression()?;

                while self.tok_is_one_of(&[Bar, Kw(To), Kw(Downto)]) {
                    if self.tok_is(Bar) {
                        choices.push(expr);
                        self.advance_tok();
                        expr = self.parse_expression()?;
                    } else {
                        let dir = Direction::from(self.kind());
                        let lhs = expr;
                        let rhs = self.parse_expression()?;
                        expr = Expr::new(lhs.pos.to(&rhs.pos), ExprKind::Range(
                            RangeExpr {
                                lhs: Box::new(lhs),
                                dir,
                                rhs: Box::new(rhs),
                            }
                        ));

                    }
                }

                if self.tok_is(EqGt) {
                    choices.push(expr);

                    self.advance_tok();

                    let designator = self.parse_expression()?;
                    expr = Expr::new(start.to(&designator.pos), ExprKind::Assoc(
                        AssocExpr {
                            choices,
                            designator: Box::new(designator),
                        }
                    ));

                } else if !choices.is_empty() {
                    return self.expr_choices_without_designator();
                }

                aggregates.push(expr);

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }

            if self.tok_is(RParen) {
                self.advance_tok(); // Eat )
            }

            let pos = start.to(&self.pos());
            let expr = Expr::new(pos, ExprKind::Aggregate(aggregates));

            return Ok(expr);
        }



        unimplemented!();
    }

    fn parse_expr_with_precedence(&mut self, prec: u32) -> PResult<Expr> {
        let start = self.pos();
        let lhs = self.parse_expr_atom()?;

        let op = Op::from_token(&self.tok);
        if op.is_none() {
            return Ok(lhs);
        }
        let op = op.unwrap();

        if op.precedence() < prec {
            return Ok(lhs);
        }

        let next_prec = if op.assoc() == Assoc::Left {
            prec + 1
        } else {
            prec
        };

        let rhs = self.parse_expr_with_precedence(next_prec)?;
        let pos = start.to(&self.pos());
        let expr = Expr::new(pos, ExprKind::BinOp(BinOpExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }));
        return Ok(expr);
    }

    pub fn parse_expression(&mut self) -> PResult<Expr> {
        self.parse_expr_with_precedence(0)
    }


    pub fn parse_external_name(&mut self) -> PResult<Name> {
        unimplemented!();
    }

    pub fn parse_signature(&mut self) -> PResult<Signature> {
        debug_assert!(self.kind() == LBracket);

        let mut signature = Signature::default();
        signature.pos = self.pos();

        self.advance_tok(); // Eat [

        loop {
            if self.tok_is(RBracket) {
                break;
            } else if self.tok_is_one_of(&[Ident, CharLiteral, StringLiteral]) {
                let name = self.parse_name()?;
                signature.parameter_typenames.push(name);
            } else if self.tok_is(Kw(Return)) {
                self.advance_tok(); // Eat return
                if !self.tok_is_one_of(&[Ident, CharLiteral, StringLiteral]) {
                    return self.unexpected_tok();
                }
                let name = self.parse_name()?;
                signature.return_typename = Some(name);
            } else {
                return self.unexpected_tok();
            }
        }

        if !self.tok_is(RBracket) {
            return self.unexpected_tok();
        }

        self.advance_tok(); // Eat ]

        signature.pos = signature.pos.to(&self.last_pos);

        Ok(signature)
    }

    pub fn parse_name_prefix_segment(&mut self) -> PResult<NameSegment> {

        if !self.tok_is_one_of(&[Ident, StringLiteral, CharLiteral]) {
            return self.unexpected_tok();
        }

        let mut segment = NameSegment {
            pos: self.pos(),
            kind: SegmentKind::Identifier,
        };

        if !self.tok_is(Ident) {
            let src_txt = String::from(self.scan.ctx.text_from_pos(segment.pos));
            if self.tok_is(StringLiteral) {
                if let Some(op) = Op::from_op_symbol(&src_txt) {
                    segment.kind = SegmentKind::OperatorSymbol(op);
                }
            } else if self.tok_is(CharLiteral) {
                let c = src_txt.chars().next().unwrap();
                segment.kind = SegmentKind::CharLiteral(c);
            } else {
                return self.invalid_op_symbol();
            }
        }

        self.advance_tok();

        Ok(segment)
    }

    pub fn parse_name(&mut self) -> PResult<Name> {
        if self.tok_is(LtLt) {
            return self.parse_external_name();
        }

        if !self.tok_is_one_of(&[Ident, CharLiteral, StringLiteral]) {
            return self.unexpected_tok();
        }

        let mut name = Name::default();

        name.pos = self.pos();

        let segment = self.parse_name_prefix_segment()?;
        name.segments.push(segment);


        loop {
            match self.kind() {
                LBracket => {
                    let signature = self.parse_signature()?;
                    name.segments.push(NameSegment {
                        pos: signature.pos,
                        kind: SegmentKind::Signature(signature),
                    });

                },
                Tick     => {
                    self.advance_tok(); // Eat '

                    if self.tok_is(LParen) {
                        // We found a qualified expression and not a name
                        // at all. Parse the expression as Segment and finish
                        // name parsing to convert this to an expression later.

                        let expr = self.parse_expression()?;
                        name.segments.push(NameSegment {
                            pos: expr.pos,
                            kind: SegmentKind::QualifiedExpr(expr),
                        });
                        break;
                    }

                    if self.tok_is(Ident) {
                        name.segments.push(NameSegment {
                            pos: self.pos(),
                            kind: SegmentKind::Attribute,
                        });
                        self.advance_tok();
                        continue;
                    }

                    return self.unexpected_tok();
                },
                Dot      => {
                    self.advance_tok(); // Eat .

                    if self.tok_is(Kw(All)) {
                        name.segments.push(NameSegment {
                            pos: self.pos(),
                            kind: SegmentKind::AllQualifier,
                        });

                        self.advance_tok();
                        break; // .all is always the last segment of a name
                    }

                    let segment = self.parse_name_prefix_segment()?;
                    name.segments.push(segment);

                },
                LParen   => {
                    // Until we have a better way of dealing with names with a
                    // parenthesised at the end we can't distinguish without
                    // typechecking, we just collect all tokens to parse at a
                    // later stage.
                    self.advance_tok(); // Eat (

                    let start_pos = self.pos();
                    let mut tokens = Vec::<Token>::default();
                    let mut open_count = 0;
                    while !(open_count == 0 && self.kind() == RParen) {
                        if self.kind() == EoF {
                            return self.unexpected_eof();
                        }

                        if self.kind() == LParen {
                            open_count += 1;
                        }

                        if self.kind() == RParen {
                            open_count -= 1;
                        }

                        tokens.push(self.tok.clone());
                        self.advance_tok();
                    }

                    debug_assert!(self.kind() == RParen);

                    name.segments.push( NameSegment {
                        pos: start_pos.to(&self.pos()),
                        kind: SegmentKind::UnparsedBlob(tokens),
                    });

                    self.advance_tok();
                },
                _        => break,
            };
        }

        //name.pos = name.pos.to(&name.segments.last().unwrap().pos);
        name.pos = name.pos.to(&self.pos());

        Ok(name)
    }
}
