// Parse Tokens to AST
// Author: Sebastian Schüller <schueller@ti.uni-bonn.de>

use token::*;
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
        if self.kind() == EoF {
            return self.unexpected_eof();
        }
        let err = ParseError::UnexpectedToken(
            self.tok.clone(),
            self.scan.ctx.string_at_pos(&self.pos()),
        );
        self.errors.push(err.clone());
        Err(err)
    }

    fn unexpected_eof<T>(&mut self) -> PResult<T> {
        let err = ParseError::UnexpectedEoF;
        self.errors.push(err.clone());
        Err(err)
    }

    fn invalid_op_symbol<T>(&mut self) -> PResult<T> {
        let err = ParseError::InvalidOpSymbolString;
        self.errors.push(err.clone());
        Err(err)
    }

    fn malformed_expr_err<T>(&mut self) -> PResult<T> {
        let err = ParseError::MalformedExpr;
        self.errors.push(err.clone());
        Err(err)
    }
    /*
    fn expr_choices_without_designator<T>(&mut self) -> PResult<T> {
        let err = ParseError::ExprChoicesWithoutDesignator;
        self.errors.push(err.clone());
        Err(err)
    }
    */

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

    fn eat_expect(&mut self, kind: TokenKind) -> PResult<()> {
        if !self.tok_is(kind) {
            return self.unexpected_tok();
        }
        self.advance_tok();
        Ok(())
    }

    // The expression superset is a thing to deal with
    // the many many elements the parensthised construct
    // following a name can be a part of.
    // Most of those elements are only discernable by
    // knowing the type of the preluding name.
    // The expression superset is later reduced to one
    // of the following syntax elements:
    //  element_constraint       <-- For subtype_indications
    //  (actual_parameter_part)  <-- For function_call
    //  aggregate
    //  (disrcete_range)         <-- For slice_name
    //  (expr {,expr})           <-- For indexed_name
    //  (expr)                   <-- For type_conversions
    //
    fn parse_expr_superset(&mut self) -> PResult<Expr> {
        debug_assert!(self.kind() == LParen);

        let start = self.pos();

        let mut exprs = Vec::<Expr>::default();

        self.eat_expect(LParen)?;
        loop {
            let mut expr = self.parse_expr_superset_element()?;

            if self.tok_is(EqGt) {
                // Check if expr is valid as the lhs of the
                // association and parse the rhs.
                if matches!(expr.kind, ExprKind::Open) ||
                    matches!(expr.kind, ExprKind::Inertial(_)) {
                    // Incomplete: Emit corresponding error
                }
                self.advance_tok();
                let rhs = self.parse_expression()?;
                expr = Expr::new(expr.pos.to(&rhs.pos), ExprKind::Assoc(AssocExpr{
                    choices: Box::new(expr),
                    designator: Box::new(rhs),
                }));
            }
            exprs.push(expr);

            if !self.tok_is(Comma) { break; }
            self.eat_expect(Comma)?;
        }
        let end = self.pos();
        self.eat_expect(RParen)?;
        debug_assert!(!exprs.is_empty());
        if exprs.len() > 1 {
            return Ok(Expr::new(start.to(&end), ExprKind::List(exprs)));
        } else {
            return Ok(exprs.pop().unwrap());
        }
    }


    fn parse_expr_superset_element(&mut self) -> PResult<Expr> {
        let start = self.pos();
        if self.tok_is(Open) {
            let end = self.pos();
            self.advance_tok();
            return Ok(Expr::new(start.to(&end), ExprKind::Open));

        } else if self.tok_is(Inertial) {
            let local_start = self.pos();
            self.advance_tok();

            let expr = self.parse_expression()?;
            let ret = Expr::new(local_start.to(&expr.pos), ExprKind::Inertial(
                Box::new(expr)
            ));
            return Ok(ret);
        }

        let mut expr = self.parse_expression()?;

        if self.tok_is_one_of(&[To, Downto, Bar]) {
            let mut choices = Vec::<Expr>::default();
            while self.tok_is_one_of(&[To, Downto, Bar]) {
                if self.tok_is_one_of(&[To, Downto]) {
                    let dir = Direction::from(self.kind());
                    self.advance_tok();
                    let rhs = self.parse_expression()?;
                    expr = Expr::new(expr.pos.to(&rhs.pos), ExprKind::Range(RangeExpr {
                        lhs: Box::new(expr),
                        dir,
                        rhs: Box::new(rhs),
                    }));
                } else {
                    self.eat_expect(Bar)?;
                    choices.push(expr);
                    expr = self.parse_expression()?;
                }
            }
            let last = expr.pos;
            choices.push(expr);
            debug_assert!(choices.len() >= 1);

            if choices.len() > 1 {
                return Ok(Expr::new(start.to(&last), ExprKind::List(choices)));
            } else {
                return Ok(choices.pop().unwrap());
            }

        } else if self.tok_can_start_name() {
            // This should be a subtype indication.
            // The first expr is allowed to be either a name or
            // parenthesised resolution indication.
            if let ExprKind::Name(name) = &expr.kind {
                if !name.is_simple() {
                    return self.malformed_expr_err();
                }

            }


            let name = self.parse_name()?;

            // TODO, Broken: Passing None to resolution indication for the
            // subtype indication is wrong, but it is a bit of a pain in the
            // ass to try converting an expression to a resolution indication.
            // We'll have to deal with this soon. - 19.06.18
            return Ok(Expr::new(start.to(&name.pos), ExprKind::SubtypeIndication(SubtypeIndication{
                pos: start.to(&name.pos),
                typemark: name,
                resolution: None,
            })));
        }

        return Ok(expr);
    }

    //#allow(dead_code)]
    fn parse_expr_atom(&mut self) -> PResult<Expr> {
        //println!("Entering parse_expr_atom on tok: {:?}", self.kind());
        self.expected.extend_from_slice(&[
            Plus, Minus, And, Or, Xor, Not, Nand,
            Nor, Xnor
        ]);
        let start = self.pos();
        if let Some(op) = Op::unary_from_token(&self.tok) {
            //println!("Parsing unary token");
            self.advance_tok();

            let rhs = self.parse_expr_with_precedence(op.precedence())?;
            let unop = ExprKind::new_unop(op, rhs);
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, unop));
        }

        if self.tok_is_one_of(&[NumDecLiteral, NumBaseLiteral]) {
            //println!("Parse NumLit");
            self.advance_tok();
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, ExprKind::NumLit(NumericLit {
                pos: range,
            })));
        }

        if self.tok_is_one_of(&[StringLiteral, BitStringLiteral]) {
            //println!("Parse StrLit");
            self.advance_tok();
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, ExprKind::StrLit(StringLit {
                pos: range,
            })));
        }

        if self.tok_is(CharLiteral) {
            //println!("Parse CharLit");
            self.advance_tok();
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, ExprKind::ChrLit(CharLit {
                pos: range,
            })));
        }

        if self.tok_is(Others) {
            //println!("Parse Others");
            self.advance_tok();
            let range = start.to(&self.pos());
            return Ok(Expr::new(range, ExprKind::Other));
        }

        if self.tok_is(Ident) {
            //println!("Parse Ident");
            let mut name = self.parse_name()?;

            // An Ident might also signify a qualified expression,
            // so we deconstruct the name to get the expression.
            if name.is_qualifiend_expr() {
                let expr_segment = name.segments.pop().unwrap();
                let expr = expr_segment.kind.unwrap_qualified_expr();

                name.pos = name.pos.to(&name.segments.last().unwrap().pos);
                let range = start.to(&expr.pos);
                return Ok(Expr::new(range, ExprKind::Qualified(QualifiedExpr {
                    qualifier: name,
                    expr: expr,
                })));

            }


            let range = start.to(&name.pos);
            return Ok(Expr::new(range, ExprKind::Name(name)));

        }


        if self.tok_is(LParen) {
            return self.parse_expr_superset();
        }

        return self.unexpected_tok();

    }

    fn parse_expr_with_precedence(&mut self, prec: u32) -> PResult<Expr> {
        //println!("Entering parse_expr_with_precedence on tok {:?}: {}", self.kind(), self.scan.ctx.text_from_pos(self.pos()));
        let start = self.pos();
        let lhs = self.parse_expr_atom()?;
        //println!("Returng form parse_expr_atom on tok: {:?}", self.kind());

        let mut result = lhs;
        loop {
            //println!("Entering loop with precedence {}", prec);
            let op = Op::from_token(&self.tok);
            if op.is_none() {
                break;
            }
            let op = op.unwrap();

            if op.precedence() < prec {
                println!("Breaking loop on op with prec: {:?}: {:?}", op, op.precedence());
                break;
            }
            self.advance_tok();

            let next_prec = if op.assoc() == Assoc::Left {
                op.precedence() + 1
            } else {
                op.precedence()
            };

            let rhs = self.parse_expr_with_precedence(next_prec)?;
            let pos = start.to(&rhs.pos);
            result = Expr::new(pos, ExprKind::BinOp(BinOpExpr {
                lhs: Box::new(result),
                op,
                rhs: Box::new(rhs),
            }));
        }
        return Ok(result);
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
            } else if self.tok_is(Return) {
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

    fn tok_can_start_name(&mut self) -> bool {
        self.tok_is_one_of(&[LtLt, Ident, CharLiteral, StringLiteral])
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
                        kind: SegmentKind::Signature(Box::new(signature)),
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
                            kind: SegmentKind::QualifiedExpr(Box::new(expr)),
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

                    if self.tok_is(All) {
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

                    // Parse all parenthesised names as aggregate expressions
                    // which should be a superset of everything that is allowed
                    // to happen after a name. But maybe not.
                    // The actual checking if the stuff in the parenthesis is
                    // correct happens after the name passes typechecking.
                    //     Sebastian, 19.06.18

                    let attached_expr = self.parse_expr_superset()?;

                    name.segments.push( NameSegment {
                        pos:  attached_expr.pos,
                        kind: SegmentKind::AttachedExpression(Box::new(attached_expr)),
                    });


                    // Until we have a better way of dealing with names with a
                    // parenthesised at the end we can't distinguish without
                    // typechecking, we just collect all tokens to parse at a
                    // later stage.

                    /*
                    self.advance_tok(); // Eat (

                    let start_pos = self.pos();
                    let mut end_pos = start_pos;
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
                        end_pos = self.pos();
                        self.advance_tok();
                    }

                    debug_assert!(self.kind() == RParen);

                    name.segments.push( NameSegment {
                        pos: start_pos.to(&end_pos),
                        kind: SegmentKind::UnparsedBlob(tokens),
                    });
                    self.advance_tok();
                    */
                },
                _        => break,
            };
        }

        name.pos = name.pos.to(&name.segments.last().unwrap().pos);

        Ok(name)
    }

    pub fn parse_resolution_indication(&mut self) -> PResult<ResolutionIndication> {
        unimplemented!();
    }

    pub fn parse_subtype_indication(&mut self) -> PResult<SubtypeIndication> {
        let mut subtype = SubtypeIndication::default();
        let resolution = self.parse_resolution_indication()?;

        if self.tok_can_start_name() {
            subtype.typemark   = self.parse_name()?;
            subtype.resolution = Some(resolution);
        } else {
            if !resolution.is_function() {
            }
            subtype.typemark = resolution.try_into_name().unwrap();
            subtype.resolution = None;
        }


        unimplemented!();
    }

    pub fn parse_entity_decl(&mut self) -> PResult<EntityDecl> {
        debug_assert!(self.kind() == Entity);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) {
            return self.unexpected_tok();
        }

        let mut entity = EntityDecl::default();
        entity.name = Identifier{ pos: self.pos() };

        self.advance_tok();

        self.eat_expect(Is)?;

        if self.tok_is(Generic) {
            self.advance_tok();
            self.eat_expect(LParen)?;
            //entity.generics = self.parse_interface_list()?;
            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;
        }

        if self.tok_is(Port) {
            self.advance_tok();
            self.eat_expect(LParen)?;
            loop {
                let port_start = self.pos();
                let mut port = PortDecl::default();
                if self.tok_is(Signal) {
                    self.advance_tok();
                }

                loop {
                    if !self.tok_is(Ident) {
                        return self.unexpected_tok();
                    }
                    port.idents.push(Identifier { pos: self.pos() });

                    if !self.tok_is(Comma) { break; }
                }

                self.eat_expect(Colon)?;

                if let Some(mode) = PortMode::try_from_tokenkind(self.kind()) {
                    port.mode = mode;
                }

                port.typemark = self.parse_subtype_indication()?;

                if self.tok_is(Bus) {
                    port.is_bus = true;
                }

                if self.tok_is(ColonEq) {
                    self.advance_tok();
                    let expr = self.parse_expression()?;
                    port.default_expr = Some(Box::new(expr));
                }

                port.pos = port_start.to(&self.pos());
                if !self.tok_is(Semicolon) { break; }
            }
            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;
        }

        // Incomplete: The declerative part needs to be parsed as well.
        // (PSL statements are missing from this list)
        //     Sebastian 19.06.18
        while self.tok_is_one_of(&[Function, Procedure, Package, Type, Subtype, Constant, Signal, Shared, File, Alias, Attribute, Disconnect, Use, Group]) {
            while !self.tok_is_one_of(&[Semicolon, EoF]) { self.advance_tok(); }
            debug_assert!(self.kind() == Semicolon);
            self.advance_tok();
        }

        if self.tok_is(Begin) {
            while !self.tok_is_one_of(&[End, EoF]) { self.advance_tok(); }
        }

        self.eat_expect(End)?;

        if self.tok_is(Ident) {
            self.advance_tok();
        }

        if !self.tok_is(Semicolon) {
            return self.unexpected_tok();
        }

        entity.pos = start.to(&self.pos());
        self.advance_tok();

        Ok(entity)
    }
}

