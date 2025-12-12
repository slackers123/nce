//! High level vs. High level Typed (HL vs HLT)

use std::collections::HashMap;

use crate::{ast, codegen::Layout};

pub struct ResolvedType {
    pub name: String,
    pub layout: Layout,
}

pub struct ResolvedLocal {
    pub name: String,
    pub ty: ResolvedId,
}

pub struct ResolvedId(usize);
pub struct ConstId(usize);

pub struct Ident(String);

pub enum Const {
    Integer(u64),
    String(String),
}

pub struct Context {
    types: HashMap<Ident, ResolvedId>,
    local: HashMap<Ident, ResolvedId>,
    resolved_types: Vec<ResolvedType>,
    resolved_local: Vec<ResolvedLocal>,
    consts: Vec<Const>,
}

pub fn lower_ast_module(module: ast::Module) -> Module<Ident> {
    todo!()
}

pub fn resolve_module_types(ctx: &mut Context, module: Module<Ident>) -> Module<ResolvedId> {
    todo!()
}

pub struct Module<T> {
    definitions: Vec<Definition<T>>,
}

pub enum Definition<T> {
    Function(Function<T>),
    Variable(),
    Constant(),
}

pub struct Function<T> {
    name: String,
    args: Vec<Argument<T>>,
    return_type: Option<T>,
    body: Block<T>,
}

pub struct Argument<T> {
    name: String,
    ty: T,
}

pub struct Block<T> {
    statements: Vec<Statement<T>>,
}

pub enum Statement<T> {
    Definition(Definition<T>),
    Block(Block<T>),
    If(If<T>),
}

pub struct If<T> {
    condition: Expr<T>,
    then_block: Block<T>,
    else_ifs: Vec<(Expr<T>, Block<T>)>,
    else_block: Option<Block<T>>,
}

pub enum Expr<T> {
    Variable(T),
    Block(Block<T>),
    Const(ConstId),
    FnCall(FnCall<T>),
}

pub struct FnCall<T> {
    fun: Box<Expr<T>>,
    args: Vec<Expr<T>>,
}
