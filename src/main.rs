//! A toy language to try my hands with runtime code generation and
//! maybe making it safe to call runtime generated code?

use core::panic;
use std::{iter::Peekable, marker::PhantomData};

pub mod bc;
pub mod codegen;
pub mod parser;

#[derive(Debug, Clone, Copy)]
pub struct Span<'src> {
    start: usize,
    end: usize,
    line: usize,
    val: &'src [char],
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'src> {
    ty: TokenType,
    span: Span<'src>,
}

impl<'src> Token<'src> {
    fn new_c(pos: usize, line: usize, ty: TokenType, src: &'src [char]) -> Self {
        Token {
            ty,
            span: Span {
                start: pos,
                end: pos,
                line,
                val: src.get(pos..=pos).unwrap(),
            },
        }
    }

    fn new(start: usize, end: usize, line: usize, ty: TokenType, src: &'src [char]) -> Self {
        Token {
            ty,
            span: Span {
                start,
                end,
                line,
                val: src.get(start..=end).unwrap(),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Ident,
    Fn,
    Return,
    Let,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    SemiColon,
    Comma,
    SmallRArrow,
    StrLit,
    IntLit,
    FloatLit,
}

const KEYWORDS: &[&str] = &["fn", "return", "let"];

const KEYWORD_TYPES: &[TokenType] = &[TokenType::Fn, TokenType::Return, TokenType::Let];

pub type PToks<'src> = Peekable<Tokenizer<'src>>;

pub struct Tokenizer<'src> {
    idx: usize,
    line_cnt: usize,
    src: &'src [char],
}

impl<'src> Tokenizer<'src> {
    pub fn new(src: &'src [char]) -> Self {
        Self {
            idx: 0,
            line_cnt: 0,
            src,
        }
    }
}

pub trait TokenIter<'src> {
    fn get_next_tt(&mut self, tt: TokenType) -> Token<'src>;
}

impl<'src, T: Iterator<Item = Token<'src>>> TokenIter<'src> for T {
    fn get_next_tt(&mut self, tt: TokenType) -> Token<'src> {
        let token = self.next().unwrap();
        if token.ty != tt {
            panic!("expected {tt:?} but got {:?}", token.ty);
        }
        token
    }
}

impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        let Tokenizer { idx, src, line_cnt } = self;
        if *idx == src.len() {
            return None;
        }

        // skip whitespace
        while src.len() > *idx && src[*idx].is_whitespace() {
            if src[*idx] == '\n' {
                *line_cnt += 1;
            }
            *idx += 1;
        }

        if *idx == src.len() {
            return None;
        }

        Some(
            match src[*idx] {
                '(' => Some(TokenType::LParen),
                ')' => Some(TokenType::RParen),
                '{' => Some(TokenType::LBrace),
                '}' => Some(TokenType::RBrace),
                ':' => Some(TokenType::Colon),
                ';' => Some(TokenType::SemiColon),
                _ => None,
            }
            .map(|ty| {
                *idx += 1;
                Token::new_c(*idx - 1, *line_cnt, ty, src)
            })
            .unwrap_or_else(|| {
                if src[*idx] == '"' {
                    let start = *idx;
                    *idx += 1;
                    while src[*idx] != '"' {
                        *idx += 1;
                    }

                    *idx += 1;

                    return Token::new(start, *idx - 1, *line_cnt, TokenType::StrLit, src);
                }

                if src[*idx] == '-' && *idx < src.len() - 1 && src[*idx + 1] == '>' {
                    *idx += 2;
                    return Token::new(*idx - 1, *idx, *line_cnt, TokenType::SmallRArrow, src);
                }

                if src[*idx].is_numeric() {
                    let start = *idx;
                    while src[*idx].is_numeric() {
                        *idx += 1;
                    }
                    return Token::new(start, *idx - 1, *line_cnt, TokenType::IntLit, src);
                }

                let start = *idx;
                while src.len() > *idx && src[*idx].is_alphanumeric() {
                    *idx += 1;
                }

                let word: String = src.get(start..*idx).unwrap().iter().collect();

                if let Some(keyword) = KEYWORDS.iter().position(|w| **w == word) {
                    Token::new(start, *idx - 1, *line_cnt, KEYWORD_TYPES[keyword], src)
                } else {
                    Token::new(start, *idx - 1, *line_cnt, TokenType::Ident, src)
                }
            }),
        )
    }
}

pub trait Parseable<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self;
}

#[derive(Debug)]
pub struct Program<'src> {
    functions: Function<'src>,
}

impl<'src> Parseable<'src> for Program<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        Self {
            functions: Function::parse(toks),
        }
    }
}

#[derive(Debug)]
pub struct Function<'src> {
    name: &'src [char],
    // TODO: function arguments
    return_type: Option<Type<'src>>,
    statements: Vec<Stmt<'src>>,
}

impl<'src> Parseable<'src> for Function<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        toks.get_next_tt(TokenType::Fn);

        let name = toks.get_next_tt(TokenType::Ident).span.val;
        toks.get_next_tt(TokenType::LParen);
        // TODO: fnction args
        toks.get_next_tt(TokenType::RParen);

        let next = toks.peek().unwrap();
        let return_type = match next.ty {
            TokenType::SmallRArrow => {
                toks.next();
                Some(Type::parse(toks))
            }
            TokenType::LBrace => None,
            _ => panic!("expected either Colon or LBrace but got {:?}", next.ty),
        };

        toks.get_next_tt(TokenType::LBrace);

        let mut statements = vec![];

        while toks.peek().unwrap().ty != TokenType::RBrace {
            statements.push(Stmt::parse(toks));

            toks.get_next_tt(TokenType::SemiColon);
        }

        toks.next();

        Self {
            name,
            return_type,
            statements,
        }
    }
}

#[derive(Debug)]
pub struct Type<'src> {
    name: &'src [char],
}

impl<'src> Parseable<'src> for Type<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        let tok = toks.next().unwrap();

        if tok.ty != TokenType::Ident {
            panic!("Expected type name");
        }

        Self { name: tok.span.val }
    }
}

#[derive(Debug)]
pub enum Stmt<'src> {
    Expr(Expr<'src>),
    Return(Expr<'src>),
    /// let
    Def(),
    If(),
    While(),
}

impl<'src> Parseable<'src> for Stmt<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        match toks.peek().unwrap().ty {
            TokenType::Return => {
                toks.next();
                Self::Return(Expr::parse(toks))
            }
            TokenType::Let => Self::Def(),
            _ => Self::Expr(Expr::parse(toks)),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'src> {
    FnCall(FnCallExpr<'src>),
    Literal(LitExpr),
}

impl<'src> Parseable<'src> for Expr<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        match toks.peek().unwrap().ty {
            TokenType::FloatLit | TokenType::IntLit | TokenType::StrLit => {
                Self::Literal(LitExpr::parse(toks))
            }
            TokenType::Ident => Self::FnCall(FnCallExpr::parse(toks)),
            ty => panic!("unexpected token: {ty:?}"),
        }
    }
}

#[derive(Debug)]
pub enum LitExpr {
    String(String),
    Int(i64),
}

impl<'src> Parseable<'src> for LitExpr {
    fn parse(toks: &mut PToks<'src>) -> Self {
        let tok = toks.next().unwrap();
        let str: String = tok.span.val.iter().collect();
        match tok.ty {
            TokenType::StrLit => Self::String(str),
            TokenType::IntLit => Self::Int(str.parse().unwrap()),
            tok => panic!("unexpected token: {tok:?}"),
        }
    }
}

#[derive(Debug)]
pub struct BinExpr<'src> {
    op: BinOp,
    lhs: Box<Expr<'src>>,
    rhs: Box<Expr<'src>>,
}

#[derive(Debug)]
pub enum BinOp {
    Plus,
}

#[derive(Debug)]
pub struct PreUnExpr<'src> {
    op: PreUnOp,
    inner: Box<Expr<'src>>,
}

#[derive(Debug)]
pub enum PreUnOp {
    Neg,
}

#[derive(Debug)]
pub struct PostUnExpr<'src> {
    op: PostUnOp,
    inner: Box<Expr<'src>>,
}

#[derive(Debug)]
pub enum PostUnOp {
    Exp,
}

#[derive(Debug)]
pub struct FnCallExpr<'src> {
    name: &'src [char],
    args: Option<CommaSeparated<'src, Expr<'src>>>,
}

impl<'src> Parseable<'src> for FnCallExpr<'src> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        let name = toks.get_next_tt(TokenType::Ident).span.val;
        toks.get_next_tt(TokenType::LParen);
        let args = match toks.peek().unwrap().ty {
            TokenType::RBrace => None,
            _ => Some(CommaSeparated::parse(toks)),
        };

        toks.get_next_tt(TokenType::RParen);

        FnCallExpr { name, args }
    }
}

#[derive(Debug)]
pub struct CommaSeparated<'src, T: Parseable<'src>> {
    vals: Vec<T>,
    _int: &'src PhantomData<()>,
}

impl<'src, T: Parseable<'src>> Parseable<'src> for CommaSeparated<'src, T> {
    fn parse(toks: &mut PToks<'src>) -> Self {
        let mut vals = vec![];
        loop {
            vals.push(T::parse(toks));
            if toks.peek().unwrap().ty == TokenType::Comma {
                toks.next();
            } else {
                break;
            }
        }
        CommaSeparated {
            vals,
            _int: &PhantomData,
        }
    }
}

fn main() {
    // let src = "fn main() -> isize {
    //     return 0;
    //     }"
    // .chars()
    // .collect::<Vec<_>>();

    // let mut toks = Tokenizer::new(&src).peekable();

    // let res = Function::parse(&mut toks);

    // println!("{res:?}");

    let mut output = String::new();

    // codegen::gen_syscall(
    //     &mut output,
    //     Syscall::Write {
    //         fildes: Operand::Immediate(1),
    //         buf: Register::X1,
    //         nbytes: Operand::Immediate(13),
    //     },
    // );

    let fun = codegen::Function {
        name: "main".into(),
        args: vec![codegen::Var {
            name: "argc".into(),
            byte_size: 4,
        }],
        local_variables: vec![codegen::Var {
            name: "a".into(),
            byte_size: 8,
        }],
        local_consts: vec![codegen::Const::LiteralIsize(123)],
    };

    codegen::gen_function(&mut output, &fun);

    println!("{output}");
}
