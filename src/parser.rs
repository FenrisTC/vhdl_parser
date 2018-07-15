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

    fn tok_can_start_name(&mut self) -> bool {
        self.tok_is_one_of(&[LtLt, Ident, CharLiteral, StringLiteral])
    }

    fn tok_can_start_declaration(&mut self) -> bool {
        // (PSL statements are missing from this list)
        //     Sebastian 19.06.18
        self.tok_is_one_of(&[Function, Procedure, Package, Type,
            Subtype, Constant, Signal, Shared, File,
            Alias, Attribute, Disconnect, Pure, Impure,
            Configuration, Use, Group, Variable
        ])
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

    fn reset_to(&mut self, pos: &SrcPos) {
        self.scan.reset_to_begin_of(pos);
        self.advance_tok();
    }

    fn eat_expect(&mut self, kind: TokenKind) -> PResult<()> {
        self.expected.clear();
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
    pub fn parse_expr_superset(&mut self) -> PResult<Expr> {
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
    pub fn parse_expr_superset_element(&mut self) -> PResult<Expr> {
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

    pub fn parse_range_expr_cont(&mut self, lhs: Expr) -> PResult<RangeExpr> {
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
    pub fn parse_range(&mut self) -> PResult<Range> {
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

    pub fn parse_choices_cont(&mut self, start_expr: Expr) -> PResult<Expr> {
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

    pub fn parse_aggregate(&mut self) -> PResult<Expr> {
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
    pub fn parse_expr_atom(&mut self) -> PResult<Expr> {
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

        if self.tok_is_one_of(&[Ident, CharLiteral, StringLiteral]) {
            loop {
                if !self.tok_is_one_of(&[Ident, CharLiteral, StringLiteral]) {
                    return self.unexpected_tok();
                }
                signature.parameter_typenames.push(self.parse_name()?);
                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }
        }

        if self.tok_is(Return) {
            self.advance_tok(); // Eat return
            if !self.tok_is_one_of(&[Ident, CharLiteral, StringLiteral]) {
                return self.unexpected_tok();
            }
            let name = self.parse_name()?;
            signature.return_typename = Some(name);
        }

        self.eat_expect(RBracket)?;

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


    pub fn parse_discrete_range(&mut self) -> PResult<DiscreteRange> {
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

    pub fn parse_element_constraint(&mut self) -> PResult<ElementConstraint> {
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

    pub fn parse_constraint(&mut self) -> PResult<Constraint> {
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

    pub fn parse_subprogram_decl_part(&mut self) -> PResult<SubprogramDeclPart> {
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

        if self.tok_is(Is) {
            self.advance_tok();
            self.eat_expect(New)?;
            // Refactor: Refer to name parsing question in parse_alias_decl wrt. signature
            // parsing.
            let name = Box::new(self.parse_name()?);
            self.eat_expect(Generic)?;
            self.eat_expect(Map)?;
            let generic_maps = self.parse_association_list()?;
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);
            return Ok(SubprogramDeclPart::Inst(SubprogramInstDecl { pos, designator, name, generic_maps }));
        }

        let (generics, generic_maps) = if self.tok_is(Generic) {
            let generics = Some(self.parse_generic_list()?);
            let maps = if self.tok_is(Generic) {
                self.advance_tok();
                self.eat_expect(Map)?;
                Some(self.parse_association_list()?)
            } else { None };

            (generics, maps)
        } else { (None, None) };


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

        let kind = if is_function {
            if return_type.is_none() { return self.err(ParseError::NoReturnInFunction); }

            SubprogramKind::Function {
                is_pure: is_pure.unwrap_or(true),
                return_type: Box::new(return_type.unwrap()),
            }
        } else {
            if is_pure.is_some()     { return self.err(ParseError::PurityInProcedure); }
            if return_type.is_some() { return self.err(ParseError::ReturnInProcedure); }
            SubprogramKind::Procedure
        };


        if self.tok_is(Semicolon) {
            self.advance_tok();
            let pos = start.to(&self.last_pos);
            let spec = SubprogramSpec { pos, kind, designator, generics, generic_maps, parameters, };
            return Ok(SubprogramDeclPart::Decl(spec));
        }

        self.eat_expect(Is)?;
        unimplemented!("We still need to implement the subprogram body parsing. Make this a priority if when we know how to deal with declarations and when we can parse statements");

    }

    pub fn parse_port_list(&mut self) -> PResult<Vec<PortDeclaration>> {
        debug_assert!(self.tok.kind == Port);
        // port_clause ::= _port_ ( port_list );
        // port_list   ::= interface_signal_declaration {; interface_signal_declaration}
        self.advance_tok();
        self.eat_expect(LParen)?;
        let mut ports = Vec::<PortDeclaration>::default();
        loop {
            let port = self.parse_port_declaration()?;
            ports.push(port);
            if !self.tok_is(Semicolon) { break; }
            self.advance_tok();
        }
        self.eat_expect(RParen)?;

        Ok(ports)
    }

    pub fn parse_port_declaration(&mut self) -> PResult<PortDeclaration> {
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

    pub fn parse_association_list(&mut self) -> PResult<Vec<Expr>> {
        debug_assert!(self.tok.kind == LParen);
        self.advance_tok();
        let mut elements = Vec::<Expr>::default();
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
                elements.push(Expr::new(pos, kind));
            } else {
                elements.push(lhs);
            }

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }

        self.eat_expect(RParen)?;
        Ok(elements)
    }

    pub fn parse_interface_package_generic_map(&mut self) -> PResult<InterfacePackageGenericMap> {
        debug_assert!(self.tok.kind == Generic);

        let start = self.pos();
        self.eat_expect(Generic)?;
        self.eat_expect(Map)?;

        let map = if self.tok_is(KwDefault) {
            self.eat_expect(LParen)?;
            self.advance_tok();
            self.eat_expect(RParen)?;

            InterfacePackageGenericMap::Default(start.to(&self.last_pos))
        } else if self.tok_is(LtGt) {
            self.eat_expect(LParen)?;
            self.advance_tok();
            self.eat_expect(RParen)?;

            InterfacePackageGenericMap::Box(start.to(&self.last_pos))
        } else {
            let elements = self.parse_association_list()?;
            let pos = start.to(&self.last_pos);
            let map = GenericMapAspect { pos, elements };
            InterfacePackageGenericMap::Map(map)
        };

        Ok(map)
    }

    pub fn parse_interface_object_declaration(&mut self) -> PResult<InterfaceObjectDeclaration> {
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

    pub fn parse_interface_package_declaration(&mut self) -> PResult<InterfacePackageDeclaration> {
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

        let map = self.parse_interface_package_generic_map()?;

        Ok(InterfacePackageDeclaration {
            pos: start.to(&map.pos()),
            name,
            referred_pkg: Box::new(referred_pkg),
            map,
        })
    }

    pub fn parse_interface_subprogram_declaration(&mut self) -> PResult<InterfaceSubprogramDeclaration> {
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

            SubprogramKind::Function {
                is_pure: is_pure.unwrap_or(true),
                return_type: Box::new(return_type.unwrap()),
            }
        } else {
            if is_pure.is_some()     { return self.err(ParseError::PurityInProcedure); }
            if return_type.is_some() { return self.err(ParseError::ReturnInProcedure); }
            SubprogramKind::Procedure
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

    pub fn parse_interface_constant_declaration(&mut self) -> PResult<InterfaceConstantDeclaration> {
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

    pub fn parse_type_decl(&mut self) -> PResult<TypeDecl> {
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

    pub fn parse_subtype_decl(&mut self) -> PResult<SubtypeDecl> {
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

    pub fn parse_object_decl(&mut self) -> PResult<ObjectDecl> {
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

    pub fn parse_use_clause(&mut self) -> PResult<UseClause> {
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

    pub fn parse_disconnect_spec(&mut self) -> PResult<DisconnectSpec> {
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

    pub fn match_entity_class(&mut self) -> PResult<EntityClass> {
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

    pub fn parse_grouping_decl(&mut self) -> PResult<GroupingDecl> {
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
                let entry = self.match_entity_class()?;
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

    pub fn parse_attribute_decl_or_spec(&mut self) -> PResult<AttributeSpecOrDecl> {
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
            let class = self.match_entity_class()?;
            self.advance_tok();
            self.eat_expect(Is)?;
            let expr = Box::new(self.parse_expression()?);
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);

            return Ok(AttributeSpecOrDecl::Spec(AttributeSpec { pos, designator, specification, class, expr }));

        }
        self.eat_expect(Colon)?;


        let typemark = Box::new(self.parse_name()?);
        self.eat_expect(Semicolon)?;
        let pos = start.to(&self.last_pos);

        Ok(AttributeSpecOrDecl::Decl(AttributeDecl { pos, ident, typemark, }))
    }

    pub fn parse_alias_decl(&mut self) -> PResult<AliasDecl> {
        debug_assert!(self.tok.kind == Alias);
        let start = self.pos();
        self.advance_tok();

        let designator = if self.tok_is(Ident) {
            AliasDesignator::Ident(Identifier { pos: self.pos() })
        } else if self.tok_is(CharLiteral) {
            AliasDesignator::CharLit(CharLit { pos: self.pos() })
        } else if self.tok_is(StringLiteral) {
            {
            let op_txt = self.scan.ctx.text_from_pos(self.pos());
            println!("{}", op_txt);
            }
            if let Some(op) = Op::from_op_symbol(self.scan.ctx.text_from_pos(self.pos())) {
                AliasDesignator::OpSymbol(OperatorSymbol { pos: self.pos(), op: op })
            } else { return self.err(ParseError::StringIsNotAnOpSymbol); }
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
        // Refactor: Currently Name parsing automatically eats signatures.
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

    pub fn parse_packing_decl(&mut self) -> PResult<PackagingDecl> {
        debug_assert!(self.tok.kind == Package);
        let start = self.pos();
        self.advance_tok();

        if self.tok_is(Body) {
            self.advance_tok();
            if !self.tok_is(Ident) { return self.unexpected_tok(); }
            let ident = Identifier { pos: self.pos() };
            self.advance_tok();

            self.eat_expect(Is)?;
            let mut decls = Vec::<Declaration>::default();
            while self.tok_can_start_declaration() {
                let decl = self.parse_declaration()?;
                if !decl.is_valid_for_package_body() { return self.err(ParseError::InvalidDeclarationForPackageBody); }
                decls.push(decl);
            }
            self.eat_expect(End)?;
            if self.tok_is(Package) {
                self.advance_tok();
                self.eat_expect(Body)?;
            }
            if self.tok_is(Ident) { self.advance_tok(); }
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);
            let body = PackageBody { pos, ident, decls };
            return Ok(PackagingDecl::Body(body));
        }

        if !self.tok_is(Ident) { return self.unexpected_tok(); }
        let ident = Identifier { pos: self.pos() };
        self.advance_tok();

        self.eat_expect(Is)?;

        if self.tok_is(New) {
            self.advance_tok();
            let name = Box::new(self.parse_name()?);
            let generic_maps = if self.tok_is(Generic) {
                self.advance_tok();
                self.eat_expect(Map)?;
                Some(self.parse_association_list()?)
            } else { None };
            self.eat_expect(Semicolon)?;
            let pos = start.to(&self.last_pos);
            let inst = PackageInstDecl { pos, ident, name, generic_maps };
            return Ok(PackagingDecl::Inst(inst));
        }

        let (generics, generic_maps) = if self.tok_is(Generic) {
            self.advance_tok();
            let generics = Some(self.parse_generic_list()?);
            let generic_maps = if self.tok_is(Generic) {
                self.advance_tok();
                self.eat_expect(Map)?;
                Some(self.parse_association_list()?)
            } else { None };

            (generics, generic_maps)
        } else { (None, None) };

        let mut decls = Vec::<Declaration>::default();
        while self.tok_can_start_declaration() {
            let decl = self.parse_declaration()?;
            if !decl.is_valid_for_package_decl() { return self.err(ParseError::InvalidDeclarationForPackageDecl); }
            decls.push(decl);
        }
        self.eat_expect(End)?;

        if self.tok_is(Package) { self.advance_tok(); }
        if self.tok_is(Ident)   { self.advance_tok(); }
        self.eat_expect(Semicolon)?;

        let pos = start.to(&self.last_pos);
        let decl = PackageDecl { pos, ident, generics, generic_maps, decls };
        Ok(PackagingDecl::Decl(decl))
    }

    pub fn parse_component_decl(&mut self) -> PResult<ComponentDecl> {
        debug_assert!(self.tok.kind == Component);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) { return self.unexpected_tok(); }
        let ident = Identifier { pos: self.pos() };
        self.advance_tok();
        self.eat_expect(Is)?;

        let generics = if self.tok_is(Generic) {
            let generics = self.parse_generic_list()?;
            self.eat_expect(Semicolon)?;
            Some(generics)
        } else { None };

        let ports = if self.tok_is(Port) {
            let ports = self.parse_port_list()?;
            self.eat_expect(Semicolon)?;
            Some(ports)
        } else { None };

        self.eat_expect(End)?;
        self.eat_expect(Component)?;
        if self.tok_is(Ident) { self.advance_tok(); }
        self.eat_expect(Semicolon)?;
        let pos = start.to(&self.last_pos);

        Ok(ComponentDecl { pos, ident, generics, ports })
    }

    fn parse_vunit_binding_indication(&mut self) -> PResult<VUnitBindingIndication> {
        debug_assert!(self.tok.kind == Use);
        let start = self.pos();
        self.advance_tok();

        self.eat_expect(Vunit)?;

        let mut names = Vec::<Name>::default();
        loop {
            let name = self.parse_name()?;
            names.push(name);

            if !self.tok_is(Comma) { break; }
            self.advance_tok();
        }
        let pos = start.to(&self.last_pos);
        Ok(VUnitBindingIndication { pos, names })
    }

    fn parse_binding_indication(&mut self) -> PResult<BindingIndication> {
        let start = self.pos();
        let aspect = if self.tok_is(Use) {
            self.advance_tok();
            if self.tok_is(Open) {
                self.advance_tok();
                Some(EntityAspect { pos: start, name: None, arch: None, kind: EntityAspectKind::Open })
            } else if self.tok_is(Configuration) {
                self.advance_tok();
                let name = Some(Box::new(self.parse_selected_name()?));
                let pos = start.to(&self.last_pos);
                Some(EntityAspect { pos, name, arch: None, kind: EntityAspectKind::Configuration })
            } else if self.tok_is(Entity) {
                self.advance_tok();
                let name = Some(Box::new(self.parse_selected_name()?));
                let arch = if self.tok_is(LParen) {
                    self.advance_tok();
                    if !self.tok_is(Ident) { return self.unexpected_tok(); }
                    let ident = Identifier { pos: self.pos() };
                    self.advance_tok();
                    self.eat_expect(RParen)?;
                    Some(ident)
                } else { None };
                let pos = start.to(&self.last_pos);
                Some(EntityAspect{pos, name, arch, kind: EntityAspectKind::Entity })
            } else { None }
        } else { None };

        let generic_maps = if self.tok_is(Generic) {
            self.advance_tok();
            self.eat_expect(Map)?;
            Some(self.parse_association_list()?)
        } else { None };

        let port_maps = if self.tok_is(Port) {
            self.advance_tok();
            self.eat_expect(Map)?;
            Some(self.parse_association_list()?)
        } else { None };
        let aspect = aspect.map(|a| Box::new(a));

        let pos = start.to(&self.last_pos);
        Ok(BindingIndication { pos, aspect, generic_maps, port_maps })
    }

    fn parse_configuration_item(&mut self) -> PResult<ConfigurationItem> {
        debug_assert!(self.tok.kind == For);
        let start = self.pos();
        self.advance_tok();

        //
        // Look into ast.rs to get more info on the name parsing thing.
        // block_specification ::=
        //       name
        //     | label
        //     | label ( generate_specification )
        //
        // comonent_specification ::=
        //     Instantiation_list : component_name
        //
        // instantiation_list ::=
        //       instantiation_label { , instantiation_label }
        //     | others
        //     | all
        //

        let name = if self.tok_is(Ident) {
            let name = Box::new(self.parse_name()?);
            if !self.tok_is_one_of(&[Comma, Colon]) {
                let block_spec = name;
                let mut uses = Vec::<UseClause>::default();
                while self.tok_is(Use) {
                    let use_clause = self.parse_use_clause()?;
                    uses.push(use_clause);
                }

                let mut configs = Vec::<ConfigurationItem>::default();
                while self.tok_is(For) {
                    let config = self.parse_configuration_item()?;
                    configs.push(config);
                }

                self.eat_expect(End)?;
                self.eat_expect(For)?;
                self.eat_expect(Semicolon)?;

                let pos = start.to(&self.last_pos);

                let block = BlockConfiguration { pos, block_spec, uses, configs };
                return Ok(ConfigurationItem::Block(block));
            }
            Some(name)
        } else { None };

        let inst = if name.is_some() {
            let name = *name.unwrap();
            if !name.is_simple() { return self.unexpected_tok(); }
            // Cleanup: Maybe we want to give a bit more context about what
            // we actually try to parse.

            let mut labels = Vec::<Identifier>::default();
            labels.push(name.unwrap_ident());
            while self.tok_is(Comma) {
                self.advance_tok();
                if !self.tok_is(Ident) { return self.unexpected_tok(); }
                let ident = Identifier { pos: self.pos() };
                self.advance_tok();
                labels.push(ident);
            }
            InstantiationList::Labels(labels)
        } else if self.tok_is(All) {
            self.advance_tok();
            InstantiationList::All
        } else if self.tok_is(Others) {
            self.advance_tok();
            InstantiationList::Others
        } else {
            return self.unexpected_tok();
        };

        self.eat_expect(Colon)?;

        let name = Box::new(self.parse_name()?);
        let pos = start.to(&self.last_pos);

        let spec = Box::new(ComponentSpec { pos, inst, name });

        let bind = if self.tok_is(Use) {
            let bind = self.parse_binding_indication()?;
            self.eat_expect(Semicolon)?;
            Some(Box::new(bind))
        } else { None };

        let mut vunits = Vec::<VUnitBindingIndication>::default();
        while self.tok_is(Vunit) {
            let vunit = self.parse_vunit_binding_indication()?;
            self.eat_expect(Semicolon)?;
            vunits.push(vunit);
        }
        let vunits = if !vunits.is_empty() { Some(vunits) } else { None };


        let block = if self.tok_is(For) {
            Some(Box::new(self.parse_block_configuration()?))
        } else { None };

        self.eat_expect(End)?;
        self.eat_expect(For)?;
        self.eat_expect(Semicolon)?;

        let pos = start.to(&self.last_pos);
        let component = ComponentConfiguration { pos, spec, bind, vunits, block };

        Ok(ConfigurationItem::Component(component))
    }

    fn parse_block_configuration(&mut self) -> PResult<BlockConfiguration> {
        let start = self.pos();
        self.eat_expect(For)?;

        let block_spec = Box::new(self.parse_name()?);

        let mut uses = Vec::<UseClause>::default();
        while self.tok_is(Use) {
            let use_clause = self.parse_use_clause()?;
            uses.push(use_clause);
        }

        let mut configs = Vec::<ConfigurationItem>::default();
        while self.tok_is(For) {
            let config = self.parse_configuration_item()?;
            configs.push(config);
        }
        self.eat_expect(End)?;
        self.eat_expect(For)?;
        self.eat_expect(Semicolon)?;

        let pos = start.to(&self.last_pos);

        Ok(BlockConfiguration { pos, block_spec, uses, configs })
    }

    pub fn parse_configuration(&mut self) -> PResult<ConfigurationDecl> {
        debug_assert!(self.tok.kind == Configuration);
        let start = self.pos();
        self.advance_tok();

        if !self.tok_is(Ident) { return self.unexpected_tok(); }
        let ident = Identifier { pos: self.pos() };
        self.advance_tok();

        self.eat_expect(Of)?;

        let name = Box::new(self.parse_name()?);

        self.eat_expect(Is)?;

        let mut decls = Vec::<Declaration>::default();
        while self.tok_can_start_declaration() {
            let decl = self.parse_declaration()?;
            if !decl.is_valid_for_configuration_decl() {
                return self.err(ParseError::InvalidDeclarationForConfigurationDecl);
            }
            decls.push(decl);
        }

        let mut vunits = Vec::<VUnitBindingIndication>::default();
        while self.tok_is(Use) {
            let unit = self.parse_vunit_binding_indication()?;
            self.eat_expect(Semicolon)?;
            vunits.push(unit);
        }

        let block = Box::new(self.parse_block_configuration()?);

        self.eat_expect(End)?;
        if self.tok_is(Configuration) { self.advance_tok(); }
        if self.tok_is(Ident)         { self.advance_tok(); }
        self.eat_expect(Semicolon)?;

        let pos = start.to(&self.last_pos);

        Ok(ConfigurationDecl { pos, ident, name, decls, vunits, block })
    }

    pub fn parse_declaration(&mut self) -> PResult<Declaration> {
        let decl = if self.tok_is(Type) {
            Declaration::Type(self.parse_type_decl()?)
        }
        else if self.tok_is(Subtype) {
            Declaration::Subtype(self.parse_subtype_decl()?)
        }
        else if self.tok_is_one_of(&[Signal, Constant, File, Shared, Variable]) {
            Declaration::Object(self.parse_object_decl()?)
        }
        else if self.tok_is(Use) {
            Declaration::UseClause(self.parse_use_clause()?)
        }
        else if self.tok_is(Disconnect) {
            Declaration::Disconnect(self.parse_disconnect_spec()?)
        }
        else if self.tok_is(Group) {
            self.parse_grouping_decl()?.into()
        }
        else if self.tok_is(Attribute) {
            self.parse_attribute_decl_or_spec()?.into()
        }
        else if self.tok_is(Alias) {
            Declaration::Alias(self.parse_alias_decl()?)
        }
        else if self.tok_is_one_of(&[Function, Procedure, Pure, Impure]) {
            self.parse_subprogram_decl_part()?.into()
        }
        else if self.tok_is(Package) {
            self.parse_packing_decl()?.into()
        }
        else if self.tok_is(Configuration) {
            Declaration::Configuration(self.parse_configuration()?)
        }
        else if self.tok_is(Component) {
            Declaration::Component(self.parse_component_decl()?)
        }
        else {
            return self.unexpected_tok();
        };
        Ok(decl)
    }

    fn parse_generic_list(&mut self) -> PResult<Vec<GenericDeclaration>> {
        debug_assert!(self.tok.kind == Generic);
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
        self.eat_expect(RParen)?;
        Ok(generics)
    }

    fn parse_component_inst_cont(&mut self, kind: ComponentInstantiationKind, label: Identifier, name: Box<Name>, arch: Option<Identifier>) -> PResult<ConcurrentStatement> {
        let start = label.pos;
        let generic_maps = if self.tok_is(Generic) {
            self.advance_tok();
            self.eat_expect(Map)?;
            Some(self.parse_association_list()?)
        } else { None };

        let port_maps = if self.tok_is(Port) {
            self.advance_tok();
            self.eat_expect(Map)?;
            Some(self.parse_association_list()?)
        } else { None };

        let pos = start.to(&self.last_pos);

        let stmt = ConcurrentStatementKind::Component(ComponentInstantiationStatement {
            name, arch, kind, generic_maps, port_maps
        });
        let stmt = Box::new(stmt);


        let label = Some(label);
        let is_postponed = false;
        return Ok(ConcurrentStatement { pos, label, is_postponed, stmt, });
    }

    pub fn parse_waveform(&mut self) -> PResult<Waveform> {
        if self.tok_is(Unaffected) {
            self.advance_tok();
            Ok(Waveform::Unaffected(self.last_pos))
        } else {
            let mut forms = Vec::<WaveformElement>::default();
            loop {
                let start = self.pos();
                let expr = if self.tok_is(Null) {
                    Box::new(ExprOrNull::Null)
                } else {
                    let expr = self.parse_expression()?;
                    Box::new(ExprOrNull::Expr(expr))
                };
                let time = if self.tok_is(After) {
                    self.advance_tok();
                    Some(Box::new(self.parse_expression()?))
                } else { None };

                let pos = start.to(&self.last_pos);

                forms.push(WaveformElement { pos, expr, time });

                if !self.tok_is(Comma) { break; }
                self.advance_tok();
            }
            Ok(Waveform::Forms(forms))
        }
    }

    pub fn parse_concurrent_statement(&mut self) -> PResult<ConcurrentStatement> {
        let start = self.pos();
        let label = if self.tok_is(Ident) {
            let pos = self.pos();
            self.advance_tok();
            if self.tok_is(Colon) {
                self.advance_tok();
                Some(Identifier { pos })
            } else {
                self.reset_to(&pos);
                debug_assert!(self.tok.kind == Ident);
                None
            }
        } else { None };

        let is_postponed = if self.tok_is(Postponed) {
            self.advance_tok();
            true
        } else { false };

        if self.tok_is_one_of(&[Entity, Configuration]) { }

        if self.tok_is(Assert) {
            if is_postponed { return self.err(ParseError::PostponedArrayStmt); }
            self.advance_tok();
            let condition = Box::new(self.parse_expression()?);
            let report_expr = if self.tok_is(Report) {
                self.advance_tok();
                Some(Box::new(self.parse_expression()?))
            } else { None };
            let severity_expr = if self.tok_is(Severity) {
                self.advance_tok();
                Some(Box::new(self.parse_expression()?))
            } else { None };

            let stmt = ConcurrentStatementKind::Assert(AssertStatement { condition, report_expr, severity_expr });
            let stmt = Box::new(stmt);
            let pos = start.to(&self.last_pos);

            return Ok(ConcurrentStatement { pos, label, is_postponed, stmt });

        } else if self.tok_is(Entity) {
            self.advance_tok();
            let name = Box::new(self.parse_selected_name()?);
            let arch = if self.tok_is(LParen) {
                self.advance_tok();
                if !self.tok_is(Ident) { return self.unexpected_tok(); }
                let ident = Identifier { pos: self.pos() };
                self.advance_tok();
                self.eat_expect(RParen)?;
                Some(ident)
            } else { None };
            if label.is_none() { return self.err(ParseError::NoLabelInComponentInst); }
            if is_postponed    { return self.err(ParseError::PostponedComponentInst); }
            return self.parse_component_inst_cont(ComponentInstantiationKind::Entity, label.unwrap(), name, arch);

        } else if self.tok_is(Component) {
            self.advance_tok();
            let name = Box::new(self.parse_selected_name()?);
            if label.is_none() { return self.err(ParseError::NoLabelInComponentInst); }
            if is_postponed    { return self.err(ParseError::PostponedComponentInst); }
            return self.parse_component_inst_cont(ComponentInstantiationKind::Component, label.unwrap(), name, None);

        } else if self.tok_is(Configuration) {
            self.advance_tok();
            let name = Box::new(self.parse_selected_name()?);
            if label.is_none() { return self.err(ParseError::NoLabelInComponentInst); }
            if is_postponed    { return self.err(ParseError::PostponedComponentInst); }
            return self.parse_component_inst_cont(ComponentInstantiationKind::Configuration, label.unwrap(), name, None);

        } else if self.tok_is(Ident) {
            //
            // This seems to be a recurring theme, but if we're here
            // we can parse the actual parameter part separately, since
            // the procedure name can't contain parentheses.
            // We probably should do so…
            //     Sebastian, 13.07.18
            //
            // concurrent_procedure_call ::=
            //     [label:] [_postponed_] procedure_name [(actual_parameter_part)]
            let name_pos = self.pos();
            let name = self.parse_name()?;
            let name = Box::new(name);

            //
            // There is a ambiguity to a concurrent procedure call without
            // parameters and with a label, if the component instantiation
            // has neither generics nor ports. However, such a component seems
            // to be a bit useless. For now we just happily assume this never
            // happens.
            //     Sebastian, 13.07.18
            //
            if self.tok_is_one_of(&[Generic, Port]) {
                if label.is_none() { return self.err(ParseError::NoLabelInComponentInst); }
                if is_postponed    { return self.err(ParseError::PostponedComponentInst); }
                return self.parse_component_inst_cont(ComponentInstantiationKind::Component, label.unwrap(), name, None);
            }

            if self.tok_is(LEq) {
                //
                // concurrent_simple_signal_assignment ::=
                //     target <= [_guarded_] [delay_mechanism] waveform;
                // concurrent_conditional_signal_assignement ::=
                //     target <= [_guarded_] [delay_mechanism] conditional_waveforms;
                //
                let target = Box::new(Target::Name(*name));
                self.advance_tok();
                let is_guarded = if self.tok_is(Guarded) { self.advance_tok(); true } else { false };
                let delay = if self.tok_is(Transport) {
                    let pos = self.pos();
                    self.advance_tok();
                    Some(Box::new(DelayMechanism::Transport(pos)))
                } else if self.tok_is(Reject) {
                    self.advance_tok();
                    let expr = self.parse_expression()?;
                    self.eat_expect(Inertial)?;
                    Some(Box::new(DelayMechanism::Reject(expr)))
                } else { None };

                let wave = Box::new(self.parse_waveform()?);
                if self.tok_is(When) {
                    let condition = Box::new(self.parse_expression()?);
                    let mut choices = Vec::<ConditionalWaveform>::default();
                    choices.push ( ConditionalWaveform { pos: wave.pos().to(&condition.pos), wave, condition } );
                    let mut final_choice = None;
                    while self.tok_is(Else) {
                        self.advance_tok();
                        let wave = Box::new(self.parse_waveform()?);
                        if self.tok_is(When) {
                            let condition = Box::new(self.parse_expression()?);
                            let pos = wave.pos().to(&condition.pos);
                            choices.push( ConditionalWaveform { pos, wave, condition });
                        } else {
                            final_choice = Some(Box::new(self.parse_waveform()?));
                            break;
                        }
                    }
                    self.eat_expect(Semicolon)?;
                    let pos = name_pos.to(&self.last_pos);

                    let waveform = Box::new(ConditionalWaveforms { pos, choices, final_choice });
                    let assignement = ConcurrentConditionalSignalAssignment {
                        pos, target, is_guarded, delay, waveform
                    };
                    let pos = start.to(&self.last_pos);
                    let stmt = Box::new(ConcurrentStatementKind::ConditionalAssignment(assignement));

                    return Ok(ConcurrentStatement { pos, label, is_postponed, stmt });
                }

                self.eat_expect(Semicolon)?;
                let pos = name_pos.to(&self.last_pos);
                let waveform = wave;
                let assignment = ConcurrentSimpleSignalAssignment { pos, target, is_guarded, delay, waveform };
                let stmt = Box::new(ConcurrentStatementKind::SimpleAssignment(assignment));
                let pos = start.to(&self.last_pos);

                return Ok(ConcurrentStatement { pos, label, is_postponed, stmt });
            }

            //
            // If we only have a name, this is  a procedure call.
            //
            self.eat_expect(Semicolon)?;

            let stmt = ConcurrentStatementKind::Procedure(ProcedureCall { name });
            let stmt = Box::new(stmt);
            let pos = start.to(&self.last_pos);

            return Ok(ConcurrentStatement { pos, label, is_postponed, stmt });
        }
        unimplemented!();
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
            let generics = self.parse_generic_list()?;
            entity.generics = generics;
            self.eat_expect(Semicolon)?;
        }

        let ports = if self.tok_is(Port) {
            let ports = self.parse_port_list()?;
            self.eat_expect(Semicolon)?;
            Some(ports)
        } else { None };
        entity.ports = ports.unwrap_or(Vec::<PortDeclaration>::default());

        while self.tok_can_start_declaration() {

            let decl = self.parse_declaration()?;
            if !decl.is_valid_for_entity_decl() {
                return self.err(ParseError::InvalidDeclarationForEntity);
            }
            entity.decl_items.push(decl);
        }

        // Incomplete: Entity statement part needs to be parsed as well.
        if self.tok_is(Begin) {
            while !self.tok_is_one_of(&[End, EoF]) { self.advance_tok(); }
        }

        self.eat_expect(End)?;

        if self.tok_is(Entity) { self.advance_tok(); }
        if self.tok_is(Ident)  { self.advance_tok(); }
        self.eat_expect(Semicolon)?;

        entity.pos = start.to(&self.last_pos);

        Ok(entity)
    }
}

