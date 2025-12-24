#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub tt: TokenType,
    pub span: &'src str,
    pub line: usize,
    pub s_col: usize,
    pub e_col: usize,
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Plus,
    Minus,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Colon,
    Semicolon,
    ExclamationPoint,
    Ampersand,
    Equal,

    Fn,
    If,
    Else,
    Ident,
    Number,
}
