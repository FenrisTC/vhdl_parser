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

        let mut tok = self.scan.scan_token();
        while tok.kind == Comment { tok = self.scan.scan_token(); }
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
            self.advance_tok();
            if self.tok_is(Ident) {
                let lit  = Box::new(NumericLit { pos: start.to(&self.pos()) });
                let unit = Box::new(self.parse_name()?);
                let pos  = start.to(&unit.pos);
                return Ok(Expr::new(pos, ExprKind::PhyLit(PhysicalLit {
                    pos,
                    lit,
                    unit,
                })));
            }
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

    fn parse_expression(&mut self) -> PResult<Expr> {
        self.parse_expr_with_precedence(0)
    }

    fn parse_simple_expression(&mut self) -> PResult<Expr> {
        self.parse_expr_with_precedence(Op::Add.precedence())
    }


    fn parse_external_name(&mut self) -> PResult<Name> {
        unimplemented!();
    }

    fn parse_signature(&mut self) -> PResult<Signature> {
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

    fn parse_name_prefix_segment(&mut self) -> PResult<NameSegment> {

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

    fn parse_selected_name(&mut self) -> PResult<Name> {

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

            if self.tok_is(Tick) {
                self.advance_tok(); // Eat '

                if self.tok_is_one_of(&[Ident, KwRange]) {
                    name.add_segment(NameSegment {
                        pos: self.pos(),
                        kind: SegmentKind::Attribute,
                    });
                    self.advance_tok();
                    break;
                }

                return self.unexpected_tok();
            }

            let segment = self.parse_name_prefix_segment()?;
            name.add_segment(segment);

            if !self.tok_is_one_of(&[Dot, Tick]) { break; }
            if self.tok_is(Dot) { self.advance_tok(); }
        }

        Ok(name)
    }

    fn parse_name(&mut self) -> PResult<Name> {
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

    fn parse_resolution_indication(&mut self) -> PResult<ResolutionIndication> {
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

    fn parse_subtype_indication(&mut self) -> PResult<SubtypeIndication> {
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

    fn parse_interface_generic_map(&mut self) -> PResult<InterfaceGenericMap> {
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

    fn parse_type_decl(&mut self) -> PResult<TypeDecl> {
        debug_assert!(self.tok.kind == Type);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) {
            return self.unexpected_tok();
        }

        let typename = Identifier { pos: self.pos() };
        self.advance_tok();

        self.eat_expect(Is)?;

        // Integer, Float or Physical type
        if self.tok_is(KwRange) {
            self.advance_tok();
            let range = self.parse_range()?;

            if self.tok_is(Units) {
                // This appears to be a physical type
                self.advance_tok();
                if !self.tok_is(Ident) { return self.unexpected_tok(); }
                let primary = Identifier { pos: self.pos() };
                self.advance_tok();
                self.eat_expect(Semicolon)?;


                let mut secondaries = Vec::<SecondaryUnitDecl>::default();
                loop {
                    if !self.tok_is(Ident) { return self.unexpected_tok(); }

                    let ident = Identifier { pos: self.pos() };
                    self.advance_tok();

                    self.eat_expect(Eq)?;

                    let factor = if self.tok_is_one_of(&[NumDecLiteral, NumBaseLiteral]) {
                        let pos = self.pos();
                        self.advance_tok();
                        Some(AbstractLiteral { pos })
                    } else { None };

                    if !self.tok_is(Ident) { return self.unexpected_tok(); }
                    let unit = Identifier { pos: self.pos() };
                    self.advance_tok();

                    self.eat_expect(Semicolon)?;

                    secondaries.push(SecondaryUnitDecl { ident, factor, unit });

                    if !self.tok_is(Ident) { break; }
                }
                self.eat_expect(End)?;
                self.eat_expect(Units)?;
                if self.tok_is(Ident) { self.advance_tok(); } // Incomplete: Check name with typename
                self.eat_expect(Semicolon)?;

                let phys_def = PhysTypeDef { primary, secondaries };
                return Ok(TypeDecl {
                    pos: start.to(&self.last_pos),
                    typename,
                    def: TypeDef::Physical(Box::new(phys_def)),
                });
            } else {
                self.eat_expect(Semicolon)?;
                return Ok(TypeDecl {
                    pos: start.to(&self.last_pos),
                    typename,
                    def: TypeDef::Number(Box::new(range)),
                });
            }
        }
        // Enumeration type
        if self.tok_is(LParen) {
            self.advance_tok();
            let mut variants = Vec::<EnumVariant>::default();
            loop {
                if !self.tok_is_one_of(&[Ident, CharLiteral]) { return self.unexpected_tok(); }

                variants.push(EnumVariant { pos: self.pos() });
                self.advance_tok();

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }
            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;
            return Ok(TypeDecl {
                pos: start.to(&self.last_pos),
                typename,
                def: TypeDef::Enumeration(variants),
            });
        }
        // Array type
        if self.tok_is(Array) {
            self.advance_tok();

            self.eat_expect(LParen)?;
            let mut unbounded = Vec::<Name>::default();
            let mut is_unbounded = false;
            let mut constrained = Vec::<DiscreteRange>::default();
            let mut is_constrained = false;

            loop {
                let expr = self.parse_expression()?;

                if self.tok_is_one_of(&[To, Downto]) {
                    if is_unbounded { return self.err(ParseError::MixedArrayDefinition); }
                    let range = self.parse_range_expr_cont(expr)?;
                    constrained.push(DiscreteRange::Range(range));
                    is_constrained = true;
                } else if self.tok_is(KwRange) {
                    if is_constrained { return self.err(ParseError::MixedArrayDefinition); }
                    if !expr.is_name() { return self.err(ParseError::MalformedArrayDefinition); }
                    self.advance_tok();

                    if self.tok_is(LtGt) {
                        self.advance_tok();
                        let name = expr.unwrap_name();
                        unbounded.push(name);
                        is_unbounded = true;
                    } else {
                        // Cleanup: this is a bit ugly, since we are already building subtype
                        // indications 10 lines or so below.
                        // Maybe we should group them together. But also, maybe not.

                        let typemark = expr.unwrap_name();
                        let constraint = Box::new(self.parse_range()?);
                        let pos   = constraint.pos();
                        let constraint = Some(Box::new(Constraint::Range { pos, constraint }));
                        let pos   = typemark.pos.to(&pos);
                        let range = DiscreteRange::SubtypeIndication(SubtypeIndication {
                            pos,
                            typemark,
                            resolution: None,
                            constraint,
                        });
                        constrained.push(range);
                        is_constrained = true;
                    }
                } else {
                    if is_unbounded { return self.err(ParseError::MixedArrayDefinition); }
                    if !expr.is_name() { return self.unexpected_tok(); }
                    let mut typemark = expr.unwrap_name();
                    let pos = typemark.pos;
                    let constraint = typemark.pop_constraint().map(|c| Box::new(c));

                    let range = if typemark.is_attribute() {
                        DiscreteRange::Attribute(typemark)
                    } else {
                        DiscreteRange::SubtypeIndication(SubtypeIndication {
                            pos,
                            typemark,
                            resolution: None,
                            constraint,
                        })
                    };
                    constrained.push(range);
                    is_constrained = true;
                }


                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }
            self.eat_expect(RParen)?;

            self.eat_expect(Of)?;

            let subtype = self.parse_subtype_indication()?;

            self.eat_expect(Semicolon)?;
            let array_def = if is_unbounded {
                ArrayDef::Unbounded(unbounded)
            } else {
                debug_assert!(is_constrained);
                ArrayDef::Constraint(constrained)
            };
            let def = ArrayTypeDef {
                def: array_def,
                subtype,
            };
            return Ok(TypeDecl {
                pos: start.to(&self.last_pos),
                typename,
                def: TypeDef::Array(Box::new(def)),
            });

        }
        // Record type
        if self.tok_is(Record) {
            self.advance_tok();
            let mut elements = Vec::<(Vec<Identifier>, Box<SubtypeIndication>)>::default();
            loop {
                let mut idents = Vec::<Identifier>::default();
                loop {
                    if !self.tok_is(Ident) { return self.unexpected_tok(); }
                    idents.push(Identifier { pos: self.pos() });
                    self.advance_tok();

                    if !self.tok_is(Comma) { break; }
                    self.advance_tok();
                }
                self.eat_expect(Colon)?;
                let subtype = self.parse_subtype_indication()?;
                elements.push((idents, Box::new(subtype)));
                self.eat_expect(Semicolon)?;

                if !self.tok_is(Ident) { break; }
            }
            self.eat_expect(End)?;
            self.eat_expect(Record)?;
            if self.tok_is(Ident) { // Incomplete: Check that ident matches typename!
                self.advance_tok();
            }
            self.eat_expect(Semicolon)?;
            return Ok(TypeDecl {
                pos: start.to(&self.last_pos),
                typename,
                def: TypeDef::Record(elements),
            });
        }
        // Access type
        if self.tok_is(Access) {
            self.advance_tok();
            let subtype = self.parse_subtype_indication()?;
            self.eat_expect(Semicolon)?;
            return Ok(TypeDecl {
                pos: start.to(&self.last_pos),
                typename,
                def: TypeDef::Access(Box::new(subtype)),
            });
        }
        // File type
        if self.tok_is(File) {
            self.advance_tok();
            self.eat_expect(Of)?;
            let filetype = Box::new(self.parse_selected_name()?);
            self.eat_expect(Semicolon)?;
            return Ok(TypeDecl {
                pos: start.to(&self.last_pos),
                typename,
                def: TypeDef::File(filetype),
            });
        }
        // Protected type
        // 
        // if self.tok_is(Protected) {
        //     self.advance_tok();
        // }

        self.unexpected_tok()
    }

    fn parse_subtype_decl(&mut self) -> PResult<SubtypeDecl> {
        debug_assert!(self.tok.kind == Subtype);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) { return self.unexpected_tok(); }
        let typename = Identifier { pos: self.pos() };
        self.advance_tok();

        let subtype = Box::new(self.parse_subtype_indication()?);
        let pos = start.to(&subtype.pos);

        Ok(SubtypeDecl { pos, typename, subtype, })
    }

    fn parse_object_decl(&mut self) -> PResult<ObjectDecl> {
        let start = self.pos();
        let kind = if self.tok_is(Constant) { ObjectDeclKind::Constant }
            else if self.tok_is(Signal)     { ObjectDeclKind::Signal }
            else if self.tok_is(Shared)     { ObjectDeclKind::Shared }
            else if self.tok_is(Variable)   { ObjectDeclKind::Variable }
            else if self.tok_is(File)       { ObjectDeclKind::File }
            else { return self.unexpected_tok(); };
        self.advance_tok();

        if self.tok_is(Variable) { self.advance_tok(); }

        let mut idents = Vec::<Identifier>::default();
        loop {
            if !self.tok_is(Ident) { return self.unexpected_tok(); }
            idents.push(Identifier { pos: self.pos() });
            self.advance_tok();

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(Colon)?;

        let subtype = Box::new(self.parse_subtype_indication()?);

        if self.tok_is_one_of(&[Open, Is]) {
            let file_open_kind = if self.tok_is(Open) {
                self.advance_tok();
                Some(Box::new(self.parse_expression()?))
            } else { None };

            self.eat_expect(Is)?;

            let file_open_name = Some(Box::new(self.parse_name()?));
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);

            return Ok(ObjectDecl {
                pos, kind, idents, subtype,
                default: None, signal_kind: None,
                file_open_kind, file_open_name
            });

        }

        let signal_kind = if self.tok_is_one_of(&[Register, Bus]) {
            if kind != ObjectDeclKind::Signal { return self.err(ParseError::SignalKindInNonSignalDecl); }
            let signal_kind = if self.tok_is(Register) { SignalKind::Register }
                else if self.tok_is(Bus)               { SignalKind::Bus }
                else { unreachable!(); };
            self.advance_tok();
            Some(signal_kind)
        } else { None };

        let default = if self.tok_is(ColonEq) {
            self.advance_tok();
            Some(Box::new(self.parse_expression()?))
        } else { None };


        self.eat_expect(Semicolon)?;
        let pos = start.to(&self.last_pos);

        Ok(ObjectDecl {
            pos, kind, idents, subtype, default, signal_kind,
            file_open_kind: None, file_open_name: None,
        })

    }

    fn parse_use_clause(&mut self) -> PResult<UseClause> {
        debug_assert!(self.tok.kind == Use);
        let start = self.pos();
        self.advance_tok();

        let mut uses = Vec::<Name>::default();
        loop {
            uses.push(self.parse_selected_name()?);

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(Semicolon)?;
        let pos = start.to(&self.last_pos);

        Ok(UseClause { pos, uses })
    }

    fn parse_disconnect_spec(&mut self) -> PResult<DisconnectSpec> {
        debug_assert!(self.tok.kind == Disconnect);
        let start = self.pos();
        self.advance_tok();

        let signal_list = if self.tok_is(Others) { SignalList::Others }
        else if self.tok_is(All)                 { SignalList::All }
        else if self.tok_is(Ident) {
            let mut names = Vec::<Name>::default();
            loop {
                if !self.tok_is(Ident) { return self.unexpected_tok(); }
                names.push(self.parse_name()?);

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }
            SignalList::Signals(names)
        } else { return self.unexpected_tok(); };

        self.eat_expect(Colon)?;
        let typemark = Box::new(self.parse_name()?);

        self.eat_expect(After)?;
        let time = Box::new(self.parse_expression()?);

        let pos = start.to(&self.last_pos);

        Ok(DisconnectSpec { pos, signal_list, typemark, time, })
    }

    fn parse_entity_class(&mut self) -> PResult<EntityClass> {
        if self.tok_is(Entity)             { Ok(EntityClass::Entity) }
        else if self.tok_is(Architecture)  { Ok(EntityClass::Architecture) }
        else if self.tok_is(Configuration) { Ok(EntityClass::Configuration) }
        else if self.tok_is(Procedure)     { Ok(EntityClass::Procedure) }
        else if self.tok_is(Function)      { Ok(EntityClass::Function) }
        else if self.tok_is(Package)       { Ok(EntityClass::Package) }
        else if self.tok_is(Type)          { Ok(EntityClass::Type) }
        else if self.tok_is(Subtype)       { Ok(EntityClass::Subtype) }
        else if self.tok_is(Constant)      { Ok(EntityClass::Constant) }
        else if self.tok_is(Signal)        { Ok(EntityClass::Signal) }
        else if self.tok_is(Variable)      { Ok(EntityClass::Variable) }
        else if self.tok_is(Component)     { Ok(EntityClass::Component) }
        else if self.tok_is(Literal)       { Ok(EntityClass::Literal) }
        else if self.tok_is(Label)         { Ok(EntityClass::Label) }
        else if self.tok_is(Units)         { Ok(EntityClass::Units) }
        else if self.tok_is(Group)         { Ok(EntityClass::Group) }
        else if self.tok_is(File)          { Ok(EntityClass::File) }
        else if self.tok_is(Property)      { Ok(EntityClass::Property) }
        else if self.tok_is(Sequence)      { Ok(EntityClass::Sequence) }
        else { self.unexpected_tok() }
    }

    fn parse_grouping_decl(&mut self) -> PResult<GroupingDecl> {
        debug_assert!(self.tok.kind == Group);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) { return self.unexpected_tok(); }
        let ident = Identifier { pos: self.pos() };
        self.advance_tok();

        if !self.tok_is_one_of(&[Colon, Is]) { return self.unexpected_tok(); }
        if self.tok_is(Colon) {
            self.advance_tok();
            let template = Box::new(self.parse_name()?);
            let mut constituents = Vec::<Name>::default();
            self.eat_expect(LParen)?;
            loop {
                constituents.push(self.parse_name()?);

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }
            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;

            let pos = start.to(&self.last_pos);

            return Ok(GroupingDecl::Group(GroupDecl { pos, ident, template, constituents, }));

        } else {
            self.eat_expect(Is)?;
            self.eat_expect(LParen)?;
            let mut entries = Vec::<EntityClassEntry>::default();
            loop {
                let entry = self.parse_entity_class()?;
                self.advance_tok();

                let entry = if self.tok_is(LtGt) {
                    self.advance_tok();
                    EntityClassEntry::Boxed(entry)
                }
                else {
                    EntityClassEntry::Unboxed(entry)
                };

                entries.push(entry);

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }

            self.eat_expect(RParen)?;
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);

            return Ok(GroupingDecl::Template(GroupTemplateDecl { pos, ident, entries, }));
        }

    }

    fn parse_attribute_decl_or_spec(&mut self) -> PResult<AttributeSpecOrDecl> {
        debug_assert!(self.tok.kind == Attribute);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) { return self.unexpected_tok(); }
        let ident = Identifier { pos: self.pos() };
        self.advance_tok();

        if self.tok_is(Of) {
            let designator = ident;
            self.advance_tok();

            let mut specification = Vec::<EntityDesignator>::default();
            loop {
                let designator = if self.tok_is(StringLiteral) {
                    let src = self.scan.ctx.string_at_pos(&self.pos());
                    if let Some(op) = Op::from_op_symbol(&src) {
                        let pos = self.pos();
                        self.advance_tok();
                        EntityDesignator::Op(OperatorSymbol { pos, op })
                    } else {
                        return self.err(ParseError::InvalidOpSymbolString);
                    }
                } else if self.tok_is_one_of(&[CharLiteral, Ident, All, Others]) {
                    let kind = self.kind();
                    let pos = self.pos();
                    self.advance_tok();
                    match kind {
                        CharLiteral => EntityDesignator::Char(CharLit { pos }),
                        Ident       => EntityDesignator::Ident(Identifier { pos }),
                        All         => EntityDesignator::All(pos),
                        Others      => EntityDesignator::Others(pos),
                        _ => unreachable!(),
                    }
                } else {
                    return self.unexpected_tok();
                };

                specification.push(designator);

                match specification.last().unwrap() {
                    EntityDesignator::All(_) |
                    EntityDesignator::Others(_) => break,
                    _ => (),
                }

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }

            self.eat_expect(Colon)?;
            let class = self.parse_entity_class()?;
            self.eat_expect(Is)?;
            let expr = Box::new(self.parse_expression()?);
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);

            return Ok(AttributeSpecOrDecl::Spec(AttributeSpec { pos, designator, specification, class, expr }));

        }
        self.eat_expect(Colon)?;


        let typemark = Box::new(self.parse_name()?);
        let pos = start.to(&self.last_pos);

        Ok(AttributeSpecOrDecl::Decl(AttributeDecl { pos, ident, typemark, }))
    }

    fn parse_alias_decl(&mut self) -> PResult<AliasDecl> {
        debug_assert!(self.tok.kind == Alias);
        let start = self.pos();
        self.advance_tok();

        let designator = if self.tok_is(Ident) {
            AliasDesignator::Ident(Identifier { pos: self.pos() })
        } else if self.tok_is(CharLiteral) {
            AliasDesignator::CharLit(CharLit { pos: self.pos() })
        } else if let Some(op) = Op::from_op_symbol(self.scan.ctx.text_from_pos(self.pos())) {
            AliasDesignator::OpSymbol(OperatorSymbol { pos: self.pos(), op: op })
        } else {
            return self.unexpected_tok();
        };
        self.advance_tok();

        let subtype = if self.tok_is(Colon) {
            self.advance_tok();
            Some(Box::new(self.parse_subtype_indication()?))
        } else { None };

        self.eat_expect(Is)?;

        let name = Box::new(self.parse_name()?);
        // Incomplete: Currently Name parsing automatically eats signatures.
        // I'm not sure that signatures are actually allowed any time a name
        // is used in the grammar; the VHDL standard is full of weird Inconsistencies
        // like this. As the name of the thing the alias refers to however, a signature
        // is explicitly allowed (!), so we maybe should store it explicitly as well?
        // If/When we revamp name parsing, we should consider this.
        //      Sebastian 09/07/18

        self.eat_expect(Semicolon)?;
        let pos = start.to(&self.last_pos);

        Ok(AliasDecl { pos, designator, subtype, name })

    }


    fn parse_entity_decl(&mut self) -> PResult<EntityDeclaration> {
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
        let entity_decl_list = [Function, Procedure,
            Package, Type, Subtype, Constant, Signal,
            Shared, File, Alias, Attribute, Disconnect,
            Use, Group
        ];
        while self.tok_is_one_of(&entity_decl_list) {

            if self.tok_is(Type) {
                entity.decl_items.push(EntityDeclItem::TypeDecl(self.parse_type_decl()?));
            }
            else if self.tok_is(Subtype) {
                entity.decl_items.push(EntityDeclItem::SubtypeDecl(self.parse_subtype_decl()?));
            }
            else if self.tok_is_one_of(&[Signal, Constant, File, Shared]) {
                entity.decl_items.push(EntityDeclItem::ObjectDecl(self.parse_object_decl()?));
            }
            else if self.tok_is(Use) {
                entity.decl_items.push(EntityDeclItem::UseClause(self.parse_use_clause()?));
            }
            else if self.tok_is(Disconnect) {
                entity.decl_items.push(EntityDeclItem::DisconnectSpec(self.parse_disconnect_spec()?));
            }
            else if self.tok_is(Group) {
                entity.decl_items.push(EntityDeclItem::GroupingDecl(self.parse_grouping_decl()?));
            }
            else if self.tok_is(Attribute) {
                entity.decl_items.push(EntityDeclItem::Attribute(self.parse_attribute_decl_or_spec()?));
            }
            else if self.tok_is(Alias) {
                entity.decl_items.push(EntityDeclItem::AliasDecl(self.parse_alias_decl()?));
            }
            //else if self.tok_is(Function)  { }
            //else if self.tok_is(Procedure) { }
            else {
                while !self.tok_is_one_of(&[Semicolon, EoF]) { self.advance_tok(); }
                debug_assert!(self.tok.kind == Semicolon);
                self.advance_tok();
            }
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
        let ast_test = parser.parse_interface_generic_map();
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
    /*
    let tests = [
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
    */
}
