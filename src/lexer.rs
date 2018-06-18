
use token::*;
use token::TokenKind::*;

use SrcPos;

#[derive(Debug, Clone)]
pub struct ParseContext<'a> {
    pub txt: &'a str,
    pub line_offsets: Vec<usize>,
}

impl<'a> From<&'a str> for ParseContext<'a> {
    fn from(src: &'a str) -> ParseContext {
        ParseContext {
            txt: src,
            line_offsets: Vec::default(),
        }
    }
}

impl<'srcfile> ParseContext<'srcfile> {
    pub fn text_from_pos(&self, pos: SrcPos) -> &str {
        &self.txt[(pos.0 as usize) .. (pos.1 as usize)]
    }
}

#[derive(Debug)]
pub struct ScanInfo<'a> {
    pub ctx: & 'a mut ParseContext<'a>,
    pub byte_index: usize,
    pub next_ch: Option<char>,
}

impl<'a> From<&'a mut ParseContext<'a>> for ScanInfo<'a> {
    fn from(ctx: &'a mut ParseContext<'a>) -> ScanInfo<'a> {
        ScanInfo {
            ctx: ctx,
            byte_index: 0,
            next_ch: ctx.txt.chars().next(),
        }
    }
}

impl<'a> ScanInfo<'a> {
    pub fn is_eof(&self) -> bool {
        self.byte_index == self.ctx.txt.len()
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(|c| c.is_whitespace());
    }

    fn skip_while<P>(&mut self, mut predicate: P) where
        P: FnMut(char) -> bool
    {
        while let Some(c) = self.next_ch {
            if !predicate(c) {
                break;
            }
            self.advance_ch();
        }
    }

    fn ch_is<P>(&self, predicate: P) -> bool where
        P: FnMut(char) -> bool
    {
        self.next_ch.map_or(false, predicate)
    }

    fn advance_ch(&mut self) {
        if self.is_eof() { return; }
        let c = self.next_ch.unwrap();
        if c == '\n' {
            self.ctx.line_offsets.push(self.byte_index);
        }

        let new_index = self.byte_index + c.len_utf8();
        self.next_ch = self.ctx.txt[new_index..].chars().next();
        self.byte_index = new_index;
    }

    fn set_idx(&mut self, idx: usize) {
        if idx > self.ctx.txt.len() { panic!(); }
        self.byte_index = idx;
        self.next_ch    = self.ctx.txt[idx..].chars().next();
    }


    fn char_at(&self, n: usize) -> Option<char> {
        self.ctx.txt[n..].chars().next()
    }

    fn scan_extended_identifier(&mut self) -> Token {
        debug_assert!(self.next_ch == Some('\\'));
        let start = self.byte_index;

        self.skip_while(|c| c != '\\');
        if let Some(c) = self.next_ch {
            debug_assert!(c == '\\');
            self.advance_ch();
            return make_tok(start, self.byte_index, Ident);
        }
        make_tok(start, self.byte_index, EoF)
    }

    fn scan_string_literal(&mut self) -> Token {
        debug_assert!(self.next_ch == Some('"'));
        let start = self.byte_index;
        self.advance_ch();

        loop {
            let c = self.next_ch.unwrap();
            let peek_idx = self.byte_index + c.len_utf8();
            let peek     = self.char_at(peek_idx);
            match (c, peek) {
                ('"', Some('"')) => (),
                ('"', _)         => {
                    self.advance_ch();
                    return make_tok(start, self.byte_index, StringLiteral);
                },
                (_, None) => return make_tok(start, self.byte_index, Invalid), // @Error handling: this is a broken string lit. Report this to user.
                _ => (),
            }
            self.advance_ch();
        }
    }

    fn scan_bit_literal(&mut self, start: usize) -> Token {
        self.skip_while(|c| is_base_specifier(c));
        if !self.ch_is(|c| c == '"') {
            return make_tok(start, self.byte_index, Invalid);
        }
        self.advance_ch();

        if !self.ch_is(|c| is_graphic_char(c)) {
            return make_tok(start, self.byte_index, Invalid);
        }
        self.skip_while(|c| is_graphic_char(c) || c == '_');

        if !self.ch_is(|c| c == '"') {
            return make_tok(start, self.byte_index, Invalid);
        }
        self.advance_ch();

        return make_tok(start, self.byte_index, BitStringLiteral);
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.is_eof() { return make_tok(self.byte_index, self.byte_index, EoF); }

        let start = self.byte_index;
        let c = self.next_ch.unwrap();

        if c == '\\' { return self.scan_extended_identifier(); }
        if c == '"'  { return self.scan_string_literal(); }
        if is_base_specifier(c) { return self.scan_bit_literal(start); }
        if can_start_ident(c) {
            self.skip_while(|c| can_continue_ident(c));
            let end = self.byte_index;

            let s = &self.ctx.txt[start..end];

            if let Some(kind) = Token::match_keyword(s) {
                return make_tok(start, end, Kw(kind));
            }

            return make_tok(start, end, Ident);
        }

        if c.is_ascii_digit() {
            //
            // Scan for decimal literals. If it looks
            // like a base literal, scan for that instead.
            // Decimal literal rules look like this:
            //     dec_lit  ::= integer[.integer][exponent]
            //     integer  ::= digit{[_]digit}
            //     exponent ::= E[+]integer|E-integer
            //
            self.skip_while(|c| is_integer(c));
            match self.next_ch {
                Some('.') => {
                    self.advance_ch();
                    if self.next_ch.map_or(false, |c| can_start_integer(c)) {
                        return make_tok(start, self.byte_index, NumDecLiteral);
                    }
                    return make_tok(start, self.byte_index, EoF);
                },
                Some('#') => { // We found a base number literal
                    //
                    // Scan for base number literals:
                    //     base_lit ::= base#based_integer[.based_integer]#exponent
                    //     base     ::= integer
                    //     based_integer ::= extended_digit{[_]extended_digit}
                    //     extended_digit ::= digit|letter // only letters fitting the encoding
                    //
                    // At this point we already scanned the base and sit on the first '#',
                    // i.e. : base#based_integer[.based_integer]#exponent
                    //            ^
                    //
                    self.advance_ch();
                    if !self.ch_is(|c| c.is_digit(16)) {
                        return make_tok(start, self.byte_index, Invalid);
                    }
                    self.skip_while(|c| c.is_digit(16) || c == '_');

                    if self.ch_is(|c| c == '.') {
                        self.advance_ch();
                        if !self.ch_is(|c| c.is_digit(16)) {
                            return make_tok(start, self.byte_index, EoF);
                        }
                        self.skip_while(|c| c.is_digit(16) || c == '_');
                    }

                    if !self.ch_is(|c| c == '#') {
                        return make_tok(start, self.byte_index, EoF);
                    }
                    self.advance_ch();

                    if !self.ch_is(|c| c == 'e') {
                        return make_tok(start, self.byte_index, Invalid);
                    }

                    let c = self.next_ch;
                    if c == Some('+') || c == Some('-') { self.advance_ch(); }
                    if !self.ch_is(|c| can_start_integer(c)) {
                        self.skip_while(|c| is_integer(c));
                    }

                    return make_tok(start, self.byte_index, NumBaseLiteral);
                },
                Some(x) if is_base_specifier(x) => return self.scan_bit_literal(start),
                _  => (),
            }
            self.advance_ch();
            if let Some('e') = self.next_ch {
                self.advance_ch();
                let c = self.next_ch;
                if c == Some('+') || c == Some('-') { self.advance_ch(); }
                if !self.ch_is(|c| can_start_integer(c)) {
                    self.skip_while(|c| is_integer(c));
                }
            }
            return make_tok(start, self.byte_index, NumDecLiteral);
        }

        let end = self.byte_index + c.len_utf8();

        match c {
            ';' => { self.set_idx(end); return make_tok(start, end, Semicolon)},
            '.' => { self.set_idx(end); return make_tok(start, end, Dot      )},
            '(' => { self.set_idx(end); return make_tok(start, end, LParen   )},
            ')' => { self.set_idx(end); return make_tok(start, end, RParen   )},
            '[' => { self.set_idx(end); return make_tok(start, end, LBracket )},
            ']' => { self.set_idx(end); return make_tok(start, end, RBracket )},
            '{' => { self.set_idx(end); return make_tok(start, end, LBrace   )},
            '}' => { self.set_idx(end); return make_tok(start, end, RBrace   )},
            ',' => { self.set_idx(end); return make_tok(start, end, Comma    )},
            '|' => { self.set_idx(end); return make_tok(start, end, Bar      )},
            '^' => { self.set_idx(end); return make_tok(start, end, Caret    )},
            '+' => { self.set_idx(end); return make_tok(start, end, Plus     )},
            '&' => { self.set_idx(end); return make_tok(start, end, Amp      )},
            '@' => { self.set_idx(end); return make_tok(start, end, At       )},
            _   => (),
        };

        if end == self.ctx.txt.len() {
            return make_tok(start, self.byte_index, EoF);
        }

        let peek = self.char_at(end).unwrap();
        let end1 = end + peek.len_utf8();

        let mut starts_comment = false;
        match (c, peek) {
            ('-','-') |                         // Oneline comment
            ('/','*') => starts_comment = true, // Multiline comment
            ('=','>') => { self.set_idx(end1); return make_tok(start, end1, EqGt    )},
            ('*','*') => { self.set_idx(end1); return make_tok(start, end1, StarStar)},
            ('<','>') => { self.set_idx(end1); return make_tok(start, end1, LtGt    )},
            ('<','=') => { self.set_idx(end1); return make_tok(start, end1, LEq     )},
            ('<','<') => { self.set_idx(end1); return make_tok(start, end1, LtLt    )},
            ('>','>') => { self.set_idx(end1); return make_tok(start, end1, GtGt    )},
            ('>','=') => { self.set_idx(end1); return make_tok(start, end1, GEq     )},
            (':','=') => { self.set_idx(end1); return make_tok(start, end1, ColonEq )},
            ('/','=') => { self.set_idx(end1); return make_tok(start, end1, SlashEq )},
            ('=', ..) => { self.set_idx(end); return make_tok(start, end, Eq      )},
            ('*', ..) => { self.set_idx(end); return make_tok(start, end, Star    )},
            ('<', ..) => { self.set_idx(end); return make_tok(start, end, Lt      )},
            ('>', ..) => { self.set_idx(end); return make_tok(start, end, Gt      )},
            (':', ..) => { self.set_idx(end); return make_tok(start, end, Colon   )},
            ('-', ..) => { self.set_idx(end); return make_tok(start, end, Minus   )},
            ('/', ..) => { self.set_idx(end); return make_tok(start, end, Slash   )},
            _         => (),
        };

        if starts_comment {
            if c == '/' { // Multiline comment
                self.advance_ch(); // <-- Now on '*'
                self.advance_ch(); // <-- Now after '*'
                loop {
                    self.skip_while(|c| c != '*');
                    self.advance_ch();
                    let c = self.next_ch;
                    if c == None {
                        // @Incomplete: We should probably indicate that
                        // this multiline comment is never finished and thus
                        // broken.
                        return make_tok(start, self.byte_index, EoF);
                    }
                    if c == Some('/') {
                        break;
                    }
                }
            } else {        // Oneline comment
                self.skip_while(|c| c != '\n');
                self.advance_ch();
            }
            return make_tok(start, self.byte_index, Comment);
        }

        if end == self.ctx.txt.len() {
            return make_tok(start, self.byte_index, EoF);
        }

        let peek2 = self.char_at(end).unwrap();
        let end2  = end + peek2.len_utf8();

        match(c,peek,peek2) {
            ('\'',x,'\'') if is_graphic_char(x) => {
                self.set_idx(end2);
                return make_tok(start, end2, CharLiteral);
            },
            ('\'',    ..) => { self.set_idx(end);  return make_tok(start, end,  Tick)},
            ('?','<','=') => { self.set_idx(end2); return make_tok(start, end2, QLEq)},
            ('?','>','=') => { self.set_idx(end2); return make_tok(start, end2, QGEq)},
            ('?','/','=') => { self.set_idx(end2); return make_tok(start, end2, QSlashEq)},
            ('?','=', ..) => { self.set_idx(end1); return make_tok(start, end1, QEq)},
            ('?','<', ..) => { self.set_idx(end1); return make_tok(start, end1, QLt)},
            ('?','>', ..) => { self.set_idx(end1); return make_tok(start, end1, QGt)},
            ('?',     ..) => (),
            _ => (),
        };

        self.advance_ch();
        unreachable!("No Token found with state {:?}", self);
    }

}

fn make_tok(start: usize, end: usize, kind: TokenKind) -> Token {
    Token {
        kind: kind,
        pos: SrcPos(start as u32, end as u32),
    }
}




fn is_latin1_alphabetic(c: char) -> bool {
    let code = c as u32;
    if code >= 0x41 && code <=0x5A { return true; }; // Uppercase ASCII letters
    if code >= 0x61 && code <=0x7A { return true; }; // Lowercase ASCII letters
    if code >= 0xC0 && code <=0xD6 { return true; };
    if code >= 0xD8 && code <=0xDD { return true; }; // Uppercase Latin-1 letters
    if code >= 0xDF && code <=0xF6 { return true; };
    if code >= 0xF8 && code <=0xFF { return true; }; // Lowercase Latin-1 letters

    false
}

fn can_start_ident(c: char) -> bool {
    is_latin1_alphabetic(c)
}

fn can_continue_ident(c: char) -> bool {
    c == '_' || c.is_ascii_digit() || is_latin1_alphabetic(c)
}

fn is_graphic_char(c: char) -> bool {
    is_latin1_alphabetic(c) ||
        c.is_ascii_digit() ||
        c.is_ascii_whitespace() ||
        c.is_ascii_punctuation()
}

fn is_base_specifier(_c: char) -> bool {
    false
}

fn can_start_integer(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_integer(c: char) -> bool {
    c.is_ascii_digit() || c == '_'
}
