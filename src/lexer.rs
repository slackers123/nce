use std::char;

use crate::token::{Token, TokenType};

pub struct Lexer<'src> {
    source: &'src str,
    line: usize,
    col: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            line: 0,
            col: 0,
        }
    }

    pub fn skip_whitespace(&mut self) {
        let mut chars = self.source.chars();
        let mut skip = 0;
        while let Some(c) = chars.next()
            && c.is_whitespace()
        {
            self.col += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            }
            skip += c.len_utf8();
        }

        self.source = &self.source[skip..];
    }

    pub fn yeild_single_char(&self, current: char) -> Token<'src> {
        let tt = match current {
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            '<' => TokenType::LAngle,
            '>' => TokenType::RAngle,
            ':' => TokenType::Colon,
            '&' => TokenType::Ampersand,
            '=' => TokenType::Equal,
            ';' => TokenType::Semicolon,
            '!' => TokenType::ExclamationPoint,
            _ => todo!(),
        };

        Token {
            tt,
            span: &self.source[..1],
            line: self.line,
            s_col: self.col,
            e_col: self.col,
        }
    }

    pub fn yeild_identifier(&self) -> (Token<'src>, usize) {
        let mut chars = self.source.chars();

        let end = chars
            .position(|c| !c.is_alphanumeric())
            .unwrap_or(self.source.len());

        let span = &self.source[..end];

        let tt = match span {
            "fn" => TokenType::Fn,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            _ => TokenType::Ident,
        };

        (
            Token {
                tt,
                span,
                line: self.line,
                s_col: self.col,
                e_col: self.col + end,
            },
            end,
        )
    }

    pub fn yeild_number(&self) -> (Token<'src>, usize) {
        let mut chars = self.source.chars();

        let end = chars
            .position(|c| !c.is_numeric() && c != '.')
            .unwrap_or(self.source.len());

        (
            Token {
                tt: TokenType::Number,
                span: &self.source[..end],
                line: self.line,
                s_col: self.col,
                e_col: self.col + end,
            },
            end,
        )
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let mut chars = self.source.char_indices();

        let current = chars.next()?;

        let (token, size) = match current.1 {
            '+' | '-' | '(' | ')' | ':' | '&' | '<' | '>' | '{' | '}' | '=' | ';' | '!' => {
                (self.yeild_single_char(current.1), 1)
            }
            'a'..='z' | 'A'..='Z' => self.yeild_identifier(),
            '0'..='9' => self.yeild_number(),
            _ => todo!("parse {}", current.1),
        };

        self.source = &self.source[size..];
        self.col += size;

        Some(token)
    }
}
