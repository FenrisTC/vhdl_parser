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
            self.expected.clone(),
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

    fn err<T>(&mut self, err: ParseError) -> PResult<T> {
        self.errors.push(err.clone());
        Err(err)
    }


    #[allow(dead_code)]
    fn malformed_discrete_range<T>(&mut self) -> PResult<T> {
        let err = ParseError::MalformedExpr;
        self.errors.push(err.clone());
        Err(err)
    }

    /*
    fn malformed_name_err<T>(&mut self) -> PResult<T> {
        let err = ParseError::MalformedName;
        self.errors.push(err.clone());
        Err(err)
    }
    */
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

    //
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
                if !(expr.is_valid_choices() || expr.is_valid_formal()) {
                    return self.malformed_expr_err();
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


    //
    // superset_el ::=
    //       [inertial] expr
    //     | open
    //     | expr to/downto expr
    //     | subtype_indication_without_resolution
    //
    // subtype_indications may officially appear in any
    // association_list.
    // Additionally they are allowed as part of the array
    // constraint of the subtype_indication itself; however
    // this case explicitly forbids a resolution_indication
    // in it, making it a lot easier to parse.
    // I didn't find a specification for it, but I think
    // subtype_indication in association_lists are only valid
    // in generic maps, so we ignore it in the general case
    // and parse the generic map list separately.
    //      - Sebastian 26.06.18
    //
    fn parse_expr_superset_element(&mut self) -> PResult<Expr> {
        let start = self.pos();
        if self.tok_is(Open) {
            let end = self.pos();
            self.advance_tok();
            return Ok(Expr::new(start.to(&end), ExprKind::Open));

        }

        if self.tok_is(Inertial) {
            let local_start = self.pos();
            self.advance_tok();

            let expr = self.parse_expression()?;
            let ret = Expr::new(local_start.to(&expr.pos), ExprKind::Inertial(
                Box::new(expr)
            ));
            return Ok(ret);
        }

        let expr = if self.tok_is(LParen) {
            self.parse_expr_superset()
        } else {
            self.parse_expression()
        }?;

        if self.tok_is_one_of(&[To, Downto, Bar]) {
            return self.parse_choices_cont(expr);
        }

        // If we find a name or somethin in paretheses,
        // it could be a resolution_indication or typemark
        // of a subtype_indication.
        // …
        // Or a part of a record consrtaint when expr is a name
        // or any simple or selected name that is used as an actual.
        //
        if expr.is_name() || expr.is_paren() {

            // The first expression better be a valid subtype
            // resolution, because we just found something that
            // looks like a typemark.
            if self.tok_is(Ident) {
                let resolution = ResolutionIndication::try_from(expr.clone());
                if resolution.is_none() {
                    return self.malformed_expr_err();
                }

                let typemark = self.parse_selected_name()?;

                let constraint = if self.tok_is_one_of(&[KwRange, LParen]) {
                    Some(Box::new(self.parse_constraint()?))
                } else {
                    None
                };

                let indication = SubtypeIndication {
                    pos: start.to(&self.last_pos),
                    typemark,
                    resolution,
                    constraint
                };
                return Ok(Expr::new(start.to(&self.last_pos),
                    ExprKind::SubtypeIndication(indication)
                ));

            }

            // This expression looks like a typemark.
            //
            if expr.is_name() {
                let mut name = expr.unwrap_name();
                let constraint = if self.tok_is(KwRange) {
                    Some(Box::new(self.parse_constraint()?))
                } else {
                    name.pop_constraint().map(|c| Box::new(c))
                };

                //
                // Well… While this could still be a subtype indication,
                // it just looks like a regular name, so lets treat is as such.
                // If it turns out during typechecking that this actually is a subtype
                // we need to cast it at that stage.
                //
                if constraint.is_none() {
                    return Ok(Expr::new(name.pos, ExprKind::Name(name)));
                }


                let pos = name.pos.to(&self.last_pos);
                let indication = SubtypeIndication {
                    pos,
                    typemark: name,
                    resolution: None,
                    constraint: constraint,
                };

                return Ok(Expr::new(pos, ExprKind::SubtypeIndication(indication)));
            }
        }

        return Ok(expr);
    }

    fn parse_range_expr_cont(&mut self, lhs: Expr) -> PResult<RangeExpr> {
        debug_assert!(self.tok.kind == To || self.tok.kind == Downto);

        let dir: Direction = self.kind().into();
        self.advance_tok();
        let rhs = self.parse_expression()?;

        let range = RangeExpr {
            lhs: Box::new(lhs),
            dir,
            rhs: Box::new(rhs)
        };

        Ok(range)
    }

    #[allow(dead_code)]
    fn parse_range(&mut self) -> PResult<Range> {
        let lhs = self.parse_expression()?;

        let mut is_attr = false;
        if let ExprKind::Name(ref name) = lhs.kind {
            if name.is_attribute() {
                is_attr = true;
            }
        }

        if is_attr {
            let range = Range::Name(lhs.unwrap_name());
            return Ok(range);
        }

        if !self.tok_is_one_of(&[To, Downto]) {
            return self.unexpected_tok();
        }
        let range = self.parse_range_expr_cont(lhs)?;
        Ok(Range::Expr(range))
    }

    fn parse_choices_cont(&mut self, start_expr: Expr) -> PResult<Expr> {
        debug_assert!(self.tok.kind == To || self.tok.kind == Downto || self.tok.kind == Bar);
        let mut expr = start_expr;
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
        choices.push(expr);
        debug_assert!(choices.len() >= 1);

        Ok(choices.into())
    }

    fn parse_aggregate(&mut self) -> PResult<Expr> {
        debug_assert!(self.kind() == LParen);
        let start = self.pos();
        self.advance_tok();

        let mut exprs = Vec::<Expr>::default();

        loop {
            let mut lhs = self.parse_expression()?;
            if self.tok_is_one_of(&[To, Downto, Bar]) {
                 lhs = self.parse_choices_cont(lhs)?;
            }

            if self.tok_is(EqGt) {
                if !lhs.is_valid_choices() {
                    return self.malformed_expr_err();
                }

                self.advance_tok();
                let rhs = self.parse_expression()?;

                let assoc = Expr::new(lhs.pos.to(&rhs.pos), ExprKind::Assoc(
                    AssocExpr {
                        choices: Box::new(lhs),
                        designator: Box::new(rhs),
                    }
                ));
                exprs.push(assoc);
            } else {
                exprs.push(lhs);
            }

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(RParen)?;

        let inner: Expr = exprs.into();
        let outer = Expr::new_paren(start.to(&self.pos()), inner);

        Ok(outer)
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

        if self.tok_can_start_name() {
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
            let expr =  self.parse_aggregate()?;
            let lvl = 1 + expr.nesting_lvl();
            return Ok(Expr::new(expr.pos, ExprKind::Paren{ lvl, expr: Box::new(expr) }));
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

    pub fn parse_simple_expression(&mut self) -> PResult<Expr> {
        self.parse_expr_with_precedence(Op::Add.precedence())
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

    pub fn parse_selected_name(&mut self) -> PResult<Name> {

        let mut name = Name::default();

        loop {
            if self.tok_is(All) {
                name.add_segment(NameSegment {
                    pos: self.pos(),
                    kind: SegmentKind::AllQualifier 
                });
                self.advance_tok();
                break;
            }

            let segment = self.parse_name_prefix_segment()?;
            name.add_segment(segment);

            if !self.tok_is(Dot) { break; }
        }

        Ok(name)
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
        name.add_segment(segment);


        loop {
            match self.kind() {
                LBracket => {
                    let signature = self.parse_signature()?;
                    name.add_segment(NameSegment {
                        pos: signature.pos,
                        kind: SegmentKind::Signature(Box::new(signature)),
                    });


                    // attribute_name ::= prefix [signature] ' simple_name [ (expr) ]
                    //                    ~~~~~~~~~~~~~~~~~~~^
                    // alias_declaration ::= … is name [signature];
                    //                       ~~~~~~~~~~~~~~~~~~~~~^
                    if ! self.tok_is(Tick) {
                        break;
                    };
                    self.advance_tok();

                    if !self.tok_is(Ident) {
                        return self.unexpected_tok();
                    }

                    name.add_segment(NameSegment {
                        pos: self.pos(),
                        kind: SegmentKind::Identifier,
                    });

                    self.advance_tok();

                    if self.tok_is(LParen) {
                        let local_start = self.pos();
                        self.advance_tok();
                        let expr = self.parse_expression()?;
                        self.eat_expect(RParen)?;
                        name.add_segment(NameSegment {
                            pos: local_start.to(&self.pos()),
                            kind: SegmentKind::AttachedExpression(Box::new(expr)),
                        });
                    }




                },
                Tick     => {
                    self.advance_tok(); // Eat '

                    if self.tok_is(LParen) {
                        // We found a qualified expression and not a name
                        // at all. Parse the expression as Segment and finish
                        // name parsing to convert this to an expression later.

                        let expr = self.parse_expression()?;
                        name.add_segment(NameSegment {
                            pos: expr.pos,
                            kind: SegmentKind::QualifiedExpr(Box::new(expr)),
                        });
                        break;
                    }

                    if self.tok_is_one_of(&[Ident, KwRange]) {
                        name.add_segment(NameSegment {
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
                        name.add_segment(NameSegment {
                            pos: self.pos(),
                            kind: SegmentKind::AllQualifier,
                        });

                        self.advance_tok();
                        break; // .all is always the last segment of a name
                    }

                    let segment = self.parse_name_prefix_segment()?;
                    name.add_segment(segment);

                },
                LParen   => {

                    // Parse all parenthesised names as aggregate expressions
                    // which should be a superset of everything that is allowed
                    // to happen after a name. But maybe not.
                    // The actual checking if the stuff in the parenthesis is
                    // correct happens after the name passes typechecking.
                    //     Sebastian, 19.06.18

                    let attached_expr = self.parse_expr_superset()?;

                    name.add_segment( NameSegment {
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
        if self.tok_is(Ident) {
            let name = self.parse_selected_name()?;
            let resolution = ResolutionIndication::Function(
                Box::new(name)
            );
            return Ok(resolution);
        }

        let start = self.pos();
        self.eat_expect(LParen)?;

        let mut array_resolution = None;
        let mut resolutions = Vec::<(Name, ResolutionIndication)>::default();
        loop {
            let mut resolution = self.parse_resolution_indication()?;

            if self.tok_is(RParen) {
                // This is a array resolution
                resolution = ResolutionIndication::ArrayIndication {
                    pos: start.to(&self.pos()),
                    lvl: 1 + resolution.lvl(),
                    resolution: Box::new(resolution),
                };
                array_resolution = Some(resolution);
                break;
            }

            if self.tok_is_one_of(&[LParen, Ident]) {
                if let Some(name) = resolution.try_into_name() {
                    let resolution = self.parse_resolution_indication()?;
                    resolutions.push((name, resolution));
                } else {
                    return self.unexpected_tok();
                }
            }

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        let end = self.pos();
        self.eat_expect(RParen)?;

        let resolution = if let Some(resolution) = array_resolution {
            resolution
        } else {
            ResolutionIndication::RecordIndication {
                pos: start.to(&end),
                resolutions,
            }
        };

        Ok(resolution)
    }


    fn parse_discrete_range(&mut self) -> PResult<DiscreteRange> {
        let lhs = self.parse_simple_expression()?;

        if self.tok_is_one_of(&[To, Downto]) {
            let range = self.parse_range_expr_cont(lhs)?;
            return Ok(DiscreteRange::Range(range));
        }

        if self.tok_is(KwRange) {
            if !lhs.is_name() {
                return self.unexpected_tok();
            }
            let name = lhs.unwrap_name();
            let range_start = self.pos();
            self.advance_tok();
            let range = self.parse_range()?;
            let range_pos = range_start.to(&range.pos());

            let subtype_indication = SubtypeIndication {
                pos: name.pos.to(&range.pos()),
                typemark: name,
                resolution: None,
                constraint: Some(Box::new(Constraint::new_range(range_pos, range))),
            };
            return Ok(DiscreteRange::SubtypeIndication(subtype_indication));
        }

        if lhs.is_name() {
            let mut name = lhs.unwrap_name();

            if name.is_attribute() {
                return Ok(DiscreteRange::Attribute(name));
            }

            let constraint = name.pop_constraint();
            let constraint = constraint.map(|c| Box::new(c));
            let subtype_indication = SubtypeIndication {
                pos: name.pos.to(&self.pos()),
                typemark: name,
                resolution: None,
                constraint: constraint,
            };
            return Ok(DiscreteRange::SubtypeIndication(subtype_indication));
        }

        self.malformed_discrete_range()
    }

    fn parse_element_constraint(&mut self) -> PResult<ElementConstraint> {
        debug_assert!(self.tok.kind == LParen);

        let start = self.pos();
        self.eat_expect(LParen)?;
        if self.tok_is(Open) {
            self.advance_tok();

            let pos = start.to(&self.pos());
            let next = if self.tok_is(LParen) {
                Some(Box::new(self.parse_element_constraint()?))
            } else {
                None
            };

            let constraint = ElementConstraint::ArrayOpen { pos, next };

            return Ok(constraint);
        }

        let mut constraints = Vec::<DiscreteRange>::default();
        loop {
            let element = self.parse_discrete_range()?;
            constraints.push(element);

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(RParen)?;

        let next = if self.tok_is(LParen) {
            Some(Box::new(self.parse_element_constraint()?))
        } else {
            None
        };

        let pos = start.to(&self.pos());
        Ok(ElementConstraint::Array{
            pos,
            constraints,
            next,
        })
    }

    fn parse_constraint(&mut self) -> PResult<Constraint> {
        if self.tok_is(KwRange) {
            self.advance_tok();
            let range = self.parse_range()?;
            return Ok(Constraint::new_range(range.pos(), range));
        }

        let constraint = Box::new(self.parse_element_constraint()?);
        let pos = constraint.pos();
        Ok(Constraint::Element{constraint, pos})

    }

    pub fn parse_subtype_indication(&mut self) -> PResult<SubtypeIndication> {
        let mut subtype = SubtypeIndication::default();
        let resolution = self.parse_resolution_indication()?;

        if self.tok_is(Ident) {
            subtype.typemark   = self.parse_selected_name()?;
            subtype.resolution = Some(resolution);
        } else {
            if let Some(name) = resolution.try_into_name() {
                subtype.typemark = name;
                subtype.resolution = None;
            } else {
                return self.unexpected_tok();
            }
        }

        if self.tok_is_one_of(&[KwRange, LParen]) {
            let constraint = self.parse_constraint()?;
            subtype.constraint = Some(Box::new(constraint));
        }

        Ok(subtype)
    }

    fn parse_port_declaration(&mut self) -> PResult<PortDeclaration> {
        let port_start = self.pos();
        let mut port = PortDeclaration::default();
        if self.tok_is(Signal) {
            self.advance_tok();
        }

        loop {
            if !self.tok_is(Ident) {
                return self.unexpected_tok();
            }
            port.idents.push(Identifier { pos: self.pos() });
            self.advance_tok();

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(Colon)?;

        if let Some(mode) = Mode::try_from_tokenkind(self.kind()) {
            port.mode = mode;
            self.advance_tok();
        }

        port.subtype = self.parse_subtype_indication()?;

        if self.tok_is(Bus) {
            port.is_bus = true;
        }

        if self.tok_is(ColonEq) {
            self.advance_tok();
            let expr = self.parse_expression()?;
            port.default_expr = Some(Box::new(expr));
        }

        port.pos = port_start.to(&self.pos());
        Ok(port)
    }

    pub fn parse_interface_generic_map(&mut self) -> PResult<InterfaceGenericMap> {
        debug_assert!(self.tok.kind == Generic);

        let start = self.pos();
        self.eat_expect(Generic)?;
        self.eat_expect(Map)?;
        self.eat_expect(LParen)?;

        let map = if self.tok_is(KwDefault) {
            self.advance_tok();
            self.eat_expect(RParen)?;

            InterfaceGenericMap::Default(start.to(&self.last_pos))
        } else if self.tok_is(LtGt) {
            self.advance_tok();
            self.eat_expect(RParen)?;

            InterfaceGenericMap::Box(start.to(&self.last_pos))
        } else {
            let mut mappings = Vec::<Expr>::default();
            loop {
                let lhs = self.parse_expr_superset_element()?;

                if self.tok_is(EqGt) {
                    if !lhs.is_valid_formal() {
                        return self.err(ParseError::MalformedGenericMapFormal);
                    }
                    self.advance_tok();

                    let rhs = self.parse_expr_superset_element()?;
                    if !rhs.is_valid_actual() {
                        return self.err(ParseError::MalformedGenericMapActual);
                    }
                    let pos = lhs.pos.to(&rhs.pos);
                    let kind = ExprKind::Assoc(AssocExpr {
                        choices:    Box::new(lhs),
                        designator: Box::new(rhs),
                    });
                    mappings.push(Expr::new(pos, kind));
                } else {
                    mappings.push(lhs);
                }

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }

            self.eat_expect(RParen)?;
            InterfaceGenericMap::Map{pos: start.to(&self.last_pos), mappings}
        };

        Ok(map)
    }

    fn parse_interface_object_declaration(&mut self) -> PResult<InterfaceObjectDeclaration> {
        let start = self.pos();
        let class = InterfaceObjectClass::try_from_token(self.kind());
        if class.is_some() {
            self.advance_tok();
        }

        let mut idents = Vec::<Identifier>::default();
        loop {
            if !self.tok_is(Ident) { return self.unexpected_tok(); }

            idents.push(Identifier { pos: self.pos() });
            self.advance_tok();

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(Colon)?;

        let mode = Mode::try_from_tokenkind(self.kind());
        if mode.is_some() {
            self.advance_tok();
        }


        let subtype = self.parse_subtype_indication()?;

        let is_bus = if self.tok_is(Bus) { true } else { false };

        let default_expr = if self.tok_is(ColonEq) {
            self.advance_tok();
            let expr = self.parse_expression()?;
            Some(Box::new(expr))
        } else {
            None
        };

        let pos = start.to(&self.last_pos);

        let decl = InterfaceObjectDeclaration {
            pos,
            class,
            idents,
            mode,
            subtype,
            is_bus,
            default_expr,
        };

        Ok(decl)
    }

    fn parse_interface_package_declaration(&mut self) -> PResult<InterfacePackageDeclaration> {
        debug_assert!(self.tok.kind == Package);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) {
            return self.unexpected_tok();
        }

        let name = Identifier { pos: self.pos() };
        self.advance_tok();

        self.eat_expect(Is)?;
        self.eat_expect(New)?;

        let referred_pkg = self.parse_name()?;

        let map = self.parse_interface_generic_map()?;

        Ok(InterfacePackageDeclaration {
            pos: start.to(&map.pos()),
            name,
            referred_pkg: Box::new(referred_pkg),
            map,
        })
    }


    fn parse_interface_subprogram_declaration(&mut self) -> PResult<InterfaceSubprogramDeclaration> {
        debug_assert!(self.tok.kind == Pure || self.tok.kind == Impure ||
                      self.tok.kind == Procedure || self.tok.kind == Function);


        let start = self.pos();
        let is_pure = if self.tok_is(Pure) {
            self.advance_tok();
            Some(true)
        } else if self.tok_is(Impure) {
            self.advance_tok();
            Some(false)
        } else {
            None
        };

        let is_function = if self.tok_is(Procedure) {
            self.advance_tok();
            false
        } else if self.tok_is(Function) {
            self.advance_tok();
            true
        } else {
            return self.unexpected_tok();
        };

        let designator = if self.tok_is(Ident) {
            Designator::Identifier(Identifier { pos: self.pos() })
        } else if let Some(op) = Op::from_op_symbol(self.scan.ctx.text_from_pos(self.pos())) {
            Designator::OperatorSymbol(OperatorSymbol { pos: self.pos(), op: op })
        } else {
            return self.unexpected_tok();
        };
        self.advance_tok();


        let mut parameters = Vec::<InterfaceObjectDeclaration>::default();
        if self.tok_is_one_of(&[LParen, Parameter]) {
            if self.tok_is(Parameter) {
                self.advance_tok();
            }
            self.eat_expect(LParen)?;
            loop {
                let object = self.parse_interface_object_declaration()?;;
                parameters.push(object);

                if !self.tok_is(Semicolon) { break; }
                self.advance_tok();
            }
            self.eat_expect(RParen)?;
        }

        let return_type = if self.tok_is(Return) {
            self.advance_tok();
            if !self.tok_can_start_name() {
                return self.unexpected_tok();
            }
            let name = self.parse_name()?;
            Some(name)
        } else {
            None
        };

        let default = if self.tok_is(Is) {
            self.advance_tok();
            if self.tok_can_start_name() {
                let name = self.parse_name()?;
                Some(InterfaceSubprogramDefault::Name(Box::new(name)))
            } else if self.tok_is(LtGt) {
                let pos = self.pos();
                self.advance_tok();
                Some(InterfaceSubprogramDefault::Box(pos))
            } else {
                None
            }
        } else {
            None
        };

        let kind = if is_function {
            if return_type.is_none() { return self.err(ParseError::NoReturnInFunction); }

            InterfaceSubprogramKind::Function {
                is_pure: is_pure.unwrap_or(true),
                return_type: Box::new(return_type.unwrap()),
            }
        } else {
            if is_pure.is_some()     { return self.err(ParseError::PurityInProcedure); }
            if return_type.is_some() { return self.err(ParseError::ReturnInProcedure); }
            InterfaceSubprogramKind::Procedure
        };


        let decl = InterfaceSubprogramDeclaration {
            pos: start.to(&self.last_pos),
            kind,
            designator,
            parameters,
            default,
        };
        Ok(decl)
    }

    fn parse_interface_constant_declaration(&mut self) -> PResult<InterfaceConstantDeclaration> {
        debug_assert!(self.tok.kind == Constant || self.tok.kind == Ident);
        let mut decl = InterfaceConstantDeclaration::default();
        let start = self.pos();

        if self.tok_is(Constant) {
            self.advance_tok();
        }

        loop {
            if !self.tok_is(Ident) {
                return self.unexpected_tok();
            }
            decl.idents.push(Identifier {pos: self.pos() });
            self.advance_tok();

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(Colon)?;

        if self.tok_is(In) {
            self.advance_tok();
        }

        decl.subtype = self.parse_subtype_indication()?;
        decl.pos = start.to(&decl.subtype.pos);

        if self.tok_is(ColonEq) {
            self.advance_tok();
            let expr = self.parse_expression()?;
            decl.pos = start.to(&expr.pos);
            decl.default_expr = Some(Box::new(expr));
        }

        Ok(decl)
    }

    pub fn parse_entity_decl(&mut self) -> PResult<EntityDeclaration> {
        debug_assert!(self.kind() == Entity);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) {
            return self.unexpected_tok();
        }

        let mut entity = EntityDeclaration::default();
        entity.name = Identifier{ pos: self.pos() };

        self.advance_tok();

        self.eat_expect(Is)?;

        // generic_clause ::= _generic_ ( generic_list );
        // generic_list   ::= interface_generic_declaration {; interface_generic_declaration }
        // interface_generic_declaration ::=
        //        interface_constant_declaration
        //      | interface_type_declaration
        //      | interface_subprogram_declaration
        //      | interface_package_declaration
        if self.tok_is(Generic) {
            self.advance_tok();
            self.eat_expect(LParen)?;
            let mut generics = Vec::<GenericDeclaration>::default();
            loop {

                if self.tok_is(Package) {
                    let package = self.parse_interface_package_declaration()?;
                    let generic = GenericDeclaration::Package(package);
                    generics.push(generic);
                } else if self.tok_is(Type) {
                    self.advance_tok();
                    if !self.tok_is(Ident) {
                        return self.unexpected_tok();
                    }
                    let generic = GenericDeclaration::Type(Identifier{pos: self.pos()});
                    self.advance_tok();
                    generics.push(generic);
                } else if self.tok_is_one_of(&[Pure, Impure, Procedure, Function]) {
                    let proc = self.parse_interface_subprogram_declaration()?;
                    let generic = GenericDeclaration::Subprogram(proc);
                    generics.push(generic);
                } else if self.tok_is_one_of(&[Ident, Constant]) {
                    let constant = self.parse_interface_constant_declaration()?;
                    let generic  = GenericDeclaration::Constant(constant);
                    generics.push(generic);
                } else {
                    return self.unexpected_tok();
                }

                if !self.tok_is(Semicolon) { break; }
                self.advance_tok();
            }
            entity.generics = generics;
            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;
        }

        // port_clause ::= _port_ ( port_list );
        // port_list   ::= interface_signal_declaration {; interface_signal_declaration}
        if self.tok_is(Port) {
            self.advance_tok();
            self.eat_expect(LParen)?;
            loop {
                let port = self.parse_port_declaration()?;
                entity.ports.push(port);
                if !self.tok_is(Semicolon) { break; }
                self.advance_tok();
            }
            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;
        }

        // Incomplete: The declarative part needs to be parsed as well.
        // (PSL statements are missing from this list)
        //     Sebastian 19.06.18
        while self.tok_is_one_of(&[Function, Procedure, Package, Type, Subtype, Constant, Signal, Shared, File, Alias, Attribute, Disconnect, Use, Group]) {
            while !self.tok_is_one_of(&[Semicolon, EoF]) { self.advance_tok(); }
            debug_assert!(self.kind() == Semicolon);
            self.advance_tok();
        }

        // Incomplete: Entity statement part needs to be parsed as well.
        if self.tok_is(Begin) {
            while !self.tok_is_one_of(&[End, EoF]) { self.advance_tok(); }
        }

        self.eat_expect(End)?;

        if self.tok_is(Entity) {
            self.advance_tok();
        }

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
