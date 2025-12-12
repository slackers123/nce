//! High level vs. High level Typed (HL vs HLT)

pub struct Type(String);

pub struct Module {
    definitions: Vec<Definition>,
}

pub enum Definition {
    Function(Function),
    Variable(),
    Constant(),
}

pub struct Function {
    name: String,
    args: Vec<Argument>,
    return_type: Option<Type>,
    body: Block,
}

pub struct Argument {
    name: String,
    ty: Type,
}

pub struct Block {
    statements: Vec<Statement>,
}

pub enum Statement {
    Definition(Definition),
    Block(Block),
    If(If),
}

pub struct If {}
