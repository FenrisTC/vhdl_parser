
use token::*;
use token::Keyword::*;
use token::TokenKind::*;
use lexer::ScanInfo;


#[derive(Debug, Clone, Copy)]
pub enum NodeKind {
    LibraryClause,
    UseClause,
    Name,
    None,
}
impl Default for NodeKind {
    fn default() -> NodeKind { NodeKind::None }
}


type NodeId=usize;

#[derive(Debug, Clone, Default)]
pub struct Node {
    pub kind: NodeKind,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
}

impl Node {
    pub fn make_kind(n: NodeKind, parent: NodeId) -> Node {
        Node {
            kind: n,
            parent: parent,
            children: Vec::default(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct AstArena {
    pub nodes: Vec<Node>,
    pub imports: Vec<NodeId>,
}

impl AstArena {
    pub fn add_node(&mut self, n: Node) -> NodeId {
        self.nodes.push(n);
        self.nodes.len() - 1
    }
}

pub struct ParseInfo<'a> {
    pub scan: &'a mut ScanInfo<'a>,
    pub cached_tokens: Vec<Token>,
    pub next_tok_idx: usize,
    pub arena: AstArena,
}

impl<'a> ParseInfo<'a> {
    pub fn from_scan_info(scan: &'a mut ScanInfo<'a>) -> ParseInfo<'a> {
        ParseInfo {
            scan: scan,
            cached_tokens: Vec::default(),
            next_tok_idx: 0,
            arena: AstArena::default(),
        }
    }
}


impl<'srcfile> ParseInfo<'srcfile> {

    fn reset_arena(&mut self) {
        self.arena = AstArena::default();
    }

    fn advance_tok(&mut self) {
        if self.next_tok_idx == (self.cached_tokens.len() - 1) || self.cached_tokens.is_empty() {
            let tok = self.scan.scan_token();
            if tok.is_none() { return; };
            self.cached_tokens.push(tok.unwrap());
            self.next_tok_idx = self.cached_tokens.len() - 1;
        }

        self.next_tok_idx += 1;
    }

    fn next_tok(&self) -> Option<Token> {
        if self.cached_tokens.is_empty() { return None; };
        Some(self.cached_tokens[self.next_tok_idx])
    }


    pub fn parse_design_unit(&mut self) -> Option<AstArena> {
        self.reset_arena();

        if self.cached_tokens.is_empty() {
            self.advance_tok();
        }

        if self.next_tok().is_none() { return None; };
        match self.next_tok().unwrap().kind {
            Kw(Library) => {
                self.advance_tok();
                let library_node = Node::make_kind(NodeKind::LibraryClause, None);
                loop {
                    if self.next_tok().is_none() {
                        break;
                    }
                    let tok = self.next_tok().unwrap();
                    match tok.kind {
                        Ident => {
                        },
                        Comma => (),
                        Semicolon => break,
                        _ => (),
                    };
                    self.advance_tok();
                }
            },
            Kw(Use) => (),
            Kw(Context) => (),
            Kw(Entity) => (),
            Kw(Architecture) => (),
            Kw(Package) => (),
            Kw(Configuration) => (),
            _ => (),
        };



        None
    }
}

