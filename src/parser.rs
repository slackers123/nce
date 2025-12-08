use qparse::{
    ParseRes, char, map_err,
    multi::{many_with_separator, many_with_separator_lax, many0},
    parser::Parser,
    sequence::{delimited, preceded, trailed},
    tag::tag,
    take_while::{alpha_num0, alpha1, take_while},
    to_optional, use_first,
    whitespace::{whitespace, whitespace_wrapped},
};
use std::{collections::HashMap, fmt::Display, str::FromStr};

use crate::{
    codegen,
    mid_level::{self, RETURN_ARG_IDENT},
};

#[derive(Debug, Default)]
pub struct VM {
    global_variables: HashMap<Ident, VMVal>,
    variables: HashMap<Ident, VMVal>,
}

impl VM {
    pub fn new(global_variables: HashMap<Ident, VMVal>) -> Self {
        Self {
            global_variables,
            variables: HashMap::new(),
        }
    }

    pub fn from_existing(existing: &VM, variables: HashMap<Ident, VMVal>) -> Self {
        Self {
            global_variables: existing.global_variables.clone(),
            variables,
        }
    }

    pub fn call(&mut self, name: &str, args: Vec<VMVal>) -> Option<VMVal> {
        let fun = self
            .get_var(&Ident(name.to_string()))
            .unwrap()
            .clone()
            .as_function();

        let mut sub = VM::from_existing(
            self,
            fun.args
                .iter()
                .zip(args.into_iter())
                .map(|(name, val)| (name.ident.clone(), val))
                .collect(),
        );

        fun.eval(&mut sub)
    }

    pub fn get_var(&self, name: &Ident) -> Option<&VMVal> {
        self.variables
            .get(name)
            .or_else(|| self.global_variables.get(name))
    }

    pub fn get_var_mut(&mut self, name: &Ident) -> Option<&mut VMVal> {
        self.variables
            .get_mut(name)
            .or_else(|| self.global_variables.get_mut(name))
    }
}

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    name: Ident,
    args: Vec<TypedIdent>,
    ret_ty: Option<Type>,
    block: Block,
}

impl Function {
    pub fn eval(&self, vm: &mut VM) -> Option<VMVal> {
        self.block.eval(vm).0
    }
}

#[derive(Debug, Clone)]
pub struct TypedIdent {
    ident: Ident,
    ty: Type,
}

#[derive(Debug, Clone)]
pub struct Block {
    statements: Vec<Statement>,
}

impl Block {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        let mut last_value = None;
        for statement in &self.statements {
            let (val, return_early) = statement.eval(vm);
            if return_early {
                return (val, true);
            }

            last_value = val;
        }

        (last_value, false)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Block),
    If(RawIf),
    While(RawWhile),
    Return(RawReturn),
    Def(RawDef),
    Assign(RawAssign),
    Expression(Expr),
}

impl Statement {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        match self {
            Self::Expression(e) => e.eval(vm),
            Self::If(raw_if) => raw_if.eval(vm),
            Self::Def(raw_def) => raw_def.eval(vm),
            Self::Assign(raw_assign) => raw_assign.eval(vm),
            Self::While(raw_while) => raw_while.eval(vm),
            Self::Return(raw_return) => (raw_return.to_return.eval(vm).0, true),
            Self::Block(block) => block.eval(vm),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RawIf {
    condition: Expr,
    then_block: Block,
    else_ifs: Vec<(Expr, Block)>,
    else_block: Option<Block>,
}

impl RawIf {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        if {
            let cond_res = self.condition.eval(vm);
            if cond_res.1 {
                return cond_res;
            }
            cond_res.0.unwrap().as_bool()
        } {
            return self.then_block.eval(vm);
        }

        for else_if in &self.else_ifs {
            if {
                let cond_res = else_if.0.eval(vm);
                if cond_res.1 {
                    return cond_res;
                }
                cond_res.0.unwrap().as_bool()
            } {
                return else_if.1.eval(vm);
            }
        }

        if let Some(else_block) = &self.else_block {
            return else_block.eval(vm);
        }

        return (None, false);
    }
}

#[derive(Debug, Clone)]
pub struct RawWhile {
    condition: Expr,
    while_block: Block,
}

impl RawWhile {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        while {
            let cond_res = self.condition.eval(vm);
            if cond_res.1 {
                return cond_res;
            }
            cond_res.0.unwrap().as_bool()
        } {
            self.while_block.eval(vm);
        }

        (None, false)
    }
}

#[derive(Debug, Clone)]
pub struct RawReturn {
    to_return: Expr,
}

#[derive(Debug, Clone)]
pub struct RawDef {
    new_var: Ident,
    value: Expr,
}

impl RawDef {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        let new = self.value.eval(vm);
        if new.1 {
            return new;
        }
        vm.variables.insert(self.new_var.clone(), new.0.unwrap());

        (None, false)
    }
}

#[derive(Debug, Clone)]
pub struct RawAssign {
    target: Ident,
    value: Expr,
}

impl RawAssign {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        let val_res = self.value.eval(vm);
        if val_res.1 {
            return val_res;
        }
        *vm.get_var_mut(&self.target).unwrap() = val_res.0.unwrap();

        (None, false)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ident(String);

#[derive(Debug, Clone)]
pub struct Type(String);

#[derive(Debug, Clone)]
pub enum Expr {
    EVal(Box<Value>),
    EBin(BinExpr),
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    op: BinOp,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

impl Expr {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        match self {
            Self::EVal(v) => v.eval(vm),
            Self::EBin(BinExpr { lhs, rhs, op }) => {
                let lhs = lhs.eval(vm);
                if lhs.1 {
                    return (lhs.0, true);
                }
                let rhs = rhs.eval(vm);
                if rhs.1 {
                    return (rhs.0, true);
                }

                (
                    Some(match op {
                        BinOp::Eq => VMVal::Boolean(lhs.0.unwrap() == rhs.0.unwrap()),
                        BinOp::Neq => VMVal::Boolean(lhs.0.unwrap() != rhs.0.unwrap()),
                        BinOp::Gt => {
                            VMVal::Boolean(lhs.0.unwrap().as_num() > rhs.0.unwrap().as_num())
                        }
                        BinOp::Lt => {
                            VMVal::Boolean(lhs.0.unwrap().as_num() < rhs.0.unwrap().as_num())
                        }
                        BinOp::Gte => {
                            VMVal::Boolean(lhs.0.unwrap().as_num() >= rhs.0.unwrap().as_num())
                        }
                        BinOp::Lte => {
                            VMVal::Boolean(lhs.0.unwrap().as_num() <= rhs.0.unwrap().as_num())
                        }
                        BinOp::Add => {
                            VMVal::Number(lhs.0.unwrap().as_num() + rhs.0.unwrap().as_num())
                        }
                        BinOp::Sub => {
                            VMVal::Number(lhs.0.unwrap().as_num() - rhs.0.unwrap().as_num())
                        }
                        BinOp::Mul => {
                            VMVal::Number(lhs.0.unwrap().as_num() * rhs.0.unwrap().as_num())
                        }
                        BinOp::Div => {
                            VMVal::Number(lhs.0.unwrap().as_num() / rhs.0.unwrap().as_num())
                        }
                        BinOp::Exp => {
                            VMVal::Number(lhs.0.unwrap().as_num().powf(rhs.0.unwrap().as_num()))
                        }
                    }),
                    false,
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum VMVal {
    String(String),
    Number(f64),
    Boolean(bool),
    Function(Function),
}

impl PartialEq for VMVal {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::String(s1) => match other {
                Self::String(s2) => s1 == s2,
                _ => false,
            },
            Self::Number(n1) => match other {
                Self::Number(n2) => n1 == n2,
                _ => false,
            },
            Self::Boolean(b1) => match other {
                Self::Boolean(b2) => b1 == b2,
                _ => false,
            },
            Self::Function(f1) => match other {
                Self::Function(f2) => f1.name == f2.name,
                _ => false,
            },
        }
    }
}

impl Display for VMVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Function(fun) => write!(f, "fn {}", fun.name.0),
        }
    }
}

impl VMVal {
    pub fn as_num(self) -> f64 {
        match self {
            Self::Number(n) => n,
            _ => panic!("expected number"),
        }
    }

    pub fn as_bool(self) -> bool {
        match self {
            Self::Boolean(b) => b,
            _ => panic!("expected boolean"),
        }
    }

    pub fn as_function(self) -> Function {
        match self {
            Self::Function(f) => f,
            _ => panic!("expected boolean"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Block(Block),
    If(RawIf),
    FnCall(FnCall),
    Ident(Ident),
    Literal(Literal),
}

impl Value {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        match self {
            Self::FnCall(f) => f.eval(vm),
            Self::Literal(l) => (l.eval(vm), false),
            Self::If(v) => v.eval(vm),
            Self::Block(v) => v.eval(vm),
            Self::Ident(i) => (Some(vm.get_var(i).unwrap().clone()), false),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnCall {
    name: Ident,
    args: Vec<Expr>,
}

impl FnCall {
    pub fn eval(&self, vm: &mut VM) -> (Option<VMVal>, bool) {
        if self.name.0 == "println" {
            println!("{}", self.args[0].eval(vm).0.unwrap());
            return (None, false);
        }

        let mut args = vec![];
        for arg in &self.args {
            let a_res = arg.eval(vm);
            if a_res.1 {
                return a_res;
            }

            args.push(a_res.0.unwrap());
        }

        (vm.call(&self.name.0, args), false)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
}

impl Literal {
    pub fn eval(&self, _vm: &mut VM) -> Option<VMVal> {
        Some(match self {
            Self::Number(n) => VMVal::Number(*n),
            Self::String(s) => VMVal::String(s.clone()),
        })
    }
}

fn parse_module(input: &str) -> ParseRes<&str, Module> {
    let (input, functions) =
        whitespace_wrapped(many_with_separator(parse_function, whitespace)).parse(input)?;

    Ok((input, Module { functions }))
}

fn parse_function(input: &str) -> ParseRes<&str, Function> {
    let (input, name) = preceded((tag("fn"), whitespace), parse_ident).parse(input)?;

    let (input, args) = delimited(
        tag('('),
        many_with_separator_lax(whitespace_wrapped(parse_typed_ident), tag(',')),
        tag(')'),
    )
    .parse(input)?;

    let (input, ret_ty) = to_optional(delimited(
        whitespace_wrapped(tag("->")),
        parse_type,
        whitespace,
    ))
    .parse(input)?;

    let (input, block) = parse_block(input)?;

    Ok((
        input,
        Function {
            name,
            args,
            ret_ty,
            block,
        },
    ))
}

fn parse_block(input: &str) -> ParseRes<&str, Block> {
    let (input, statements) = delimited(
        tag('{'),
        whitespace_wrapped(many_with_separator(parse_statement, whitespace)),
        tag('}'),
    )
    .parse(input)?;

    Ok((input, Block { statements }))
}

fn parse_statement(input: &str) -> ParseRes<&str, Statement> {
    use_first([
        parse_if_stmt,
        parse_while_stmt,
        parse_return_stmt,
        parse_def_stmt,
        parse_assign_stmt,
        parse_expr_stmt,
    ])
    .parse(input)
}

fn parse_if_raw(input: &str) -> ParseRes<&str, RawIf> {
    let (input, (_, condition, then_block)) =
        (tag("if"), whitespace_wrapped(parse_basic_expr), parse_block).parse(input)?;

    let (input, else_ifs) = many0((
        preceded(
            (whitespace_wrapped(tag("else")), tag("if")),
            whitespace_wrapped(parse_basic_expr),
        ),
        parse_block,
    ))
    .parse(input)?;

    let (input, else_block) =
        to_optional(preceded(whitespace_wrapped(tag("else")), parse_block)).parse(input)?;

    Ok((
        input,
        RawIf {
            condition,
            then_block,
            else_ifs,
            else_block,
        },
    ))
}

fn parse_fn_call(input: &str) -> ParseRes<&str, FnCall> {
    let (input, (name, args)) = (
        parse_ident,
        delimited(
            whitespace_wrapped(tag('(')),
            many_with_separator_lax(parse_basic_expr, tag(',')),
            whitespace_wrapped(tag(')')),
        ),
    )
        .parse(input)?;
    Ok((input, FnCall { name, args }))
}

fn parse_literal(input: &str) -> ParseRes<&str, Literal> {
    fn parse_string_literal(input: &str) -> ParseRes<&str, Literal> {
        let (input, raw) =
            delimited(tag('"'), take_while(|c: char| c != '"'), tag('"')).parse(input)?;
        Ok((input, Literal::String(String::from(raw))))
    }

    fn parse_number_literal(input: &str) -> ParseRes<&str, Literal> {
        let (input, raw) = take_while(|c: char| c.is_ascii_digit() || c == '.').parse(input)?;
        Ok((
            input,
            Literal::Number(map_err(f64::from_str(raw))(input)?.1),
        ))
    }

    use_first([parse_string_literal, parse_number_literal]).parse(input)
}

fn parse_if_stmt(input: &str) -> ParseRes<&str, Statement> {
    let (input, raw) = parse_if_raw(input)?;
    Ok((input, Statement::If(raw)))
}

fn parse_while_stmt(input: &str) -> ParseRes<&str, Statement> {
    let (input, (_, condition, while_block)) = (
        tag("while"),
        whitespace_wrapped(parse_basic_expr),
        parse_block,
    )
        .parse(input)?;

    Ok((
        input,
        Statement::While(RawWhile {
            condition,
            while_block,
        }),
    ))
}

fn parse_return_stmt(input: &str) -> ParseRes<&str, Statement> {
    let (input, (_, to_return, _)) = (
        tag("return"),
        whitespace_wrapped(parse_basic_expr),
        tag(';'),
    )
        .parse(input)?;

    Ok((input, Statement::Return(RawReturn { to_return })))
}

fn parse_def_stmt(input: &str) -> ParseRes<&str, Statement> {
    let (input, (_, new_var, _, value, _)) = (
        tag("let"),
        whitespace_wrapped(parse_ident),
        tag('='),
        whitespace_wrapped(parse_basic_expr),
        tag(';'),
    )
        .parse(input)?;

    Ok((input, Statement::Def(RawDef { new_var, value })))
}

fn parse_assign_stmt(input: &str) -> ParseRes<&str, Statement> {
    let (input, (target, _, value, _)) = (
        whitespace_wrapped(parse_ident),
        tag('='),
        whitespace_wrapped(parse_basic_expr),
        tag(';'),
    )
        .parse(input)?;

    Ok((input, Statement::Assign(RawAssign { target, value })))
}

fn parse_expr_stmt(input: &str) -> ParseRes<&str, Statement> {
    let (input, expr) = trailed(parse_basic_expr, tag(';')).parse(input)?;

    Ok((input, Statement::Expression(expr)))
}

fn parse_typed_ident(input: &str) -> ParseRes<&str, TypedIdent> {
    let (input, (ident, _, ty)) =
        (parse_ident, whitespace_wrapped(tag(':')), parse_type).parse(input)?;

    Ok((input, TypedIdent { ident, ty }))
}

fn parse_type(input: &str) -> ParseRes<&str, Type> {
    let (input, raw) = alpha1(input)?;
    let (input, tail) = alpha_num0(input)?;

    let mut res = String::from(raw);
    res.push_str(tail);
    Ok((input, Type(res)))
}

fn parse_ident(input: &str) -> ParseRes<&str, Ident> {
    let (input, raw) = alpha1(input)?;
    let (input, tail) = alpha_num0(input)?;

    let mut res = String::from(raw);
    res.push_str(tail);
    Ok((input, Ident(res)))
}

fn parse_basic_expr(input: &str) -> ParseRes<&str, Expr> {
    parse_cmp(input)
}

fn parse_parens(input: &str) -> ParseRes<&str, Expr> {
    whitespace_wrapped(delimited(char('('), parse_cmp, char(')'))).parse(input)
}

fn parse_operation(input: &str) -> ParseRes<&str, Expr> {
    use_first([parse_parens, parse_value]).parse(input)
}

fn parse_factor(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_operation(input)?;
    let (input, exprs) = many0((whitespace_wrapped(char("^")), parse_factor)).parse(input)?;
    Ok((input, parse_expr(num1, exprs)))
}

fn parse_term(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_factor(input)?;
    let (input, exprs) = many0((
        whitespace_wrapped(use_first((char("/"), char("*")))),
        parse_factor,
    ))
    .parse(input)?;
    Ok((input, parse_expr(num1, exprs)))
}

fn parse_math_expr(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_term(input)?;
    let (input, exprs) = many0((
        whitespace_wrapped(use_first((char("+"), char("-")))),
        parse_term,
    ))
    .parse(input)?;
    Ok((input, parse_expr(num1, exprs)))
}

fn parse_cmp(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_math_expr(input)?;
    let (input, exprs) = many0((
        whitespace_wrapped(use_first([
            char("=="),
            char("!="),
            char(">"),
            char("<"),
            char(">="),
            char("<="),
        ])),
        parse_cmp,
    ))
    .parse(input)?;

    Ok((input, parse_expr(num1, exprs)))
}

fn parse_expr(expr: Expr, rem: Vec<(&str, Expr)>) -> Expr {
    rem.into_iter().fold(expr, |acc, val| parse_op(val, acc))
}

fn parse_op(tup: (&str, Expr), expr1: Expr) -> Expr {
    let (op, expr2) = tup;
    Expr::EBin(BinExpr {
        lhs: Box::new(expr1),
        rhs: Box::new(expr2),
        op: match op {
            "+" => BinOp::Add,
            "-" => BinOp::Sub,
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "^" => BinOp::Exp,
            "==" => BinOp::Eq,
            "!=" => BinOp::Neq,
            ">" => BinOp::Gt,
            "<" => BinOp::Lt,
            ">=" => BinOp::Gte,
            "<=" => BinOp::Lte,
            o => panic!("Unknown Operation: {o}"),
        },
    })
}

fn parse_value(input: &str) -> ParseRes<&str, Expr> {
    let (input, val) = use_first([
        parse_block_value,
        parse_if_value,
        parse_fn_call_value,
        parse_ident_value,
        parse_literal_value,
    ])
    .parse(input)?;

    Ok((input, Expr::EVal(Box::new(val))))
}

fn parse_block_value(input: &str) -> ParseRes<&str, Value> {
    let (input, block) = parse_block(input)?;
    Ok((input, Value::Block(block)))
}

fn parse_if_value(input: &str) -> ParseRes<&str, Value> {
    let (input, if_raw) = parse_if_raw(input)?;
    Ok((input, Value::If(if_raw)))
}

fn parse_fn_call_value(input: &str) -> ParseRes<&str, Value> {
    let (input, fn_call) = parse_fn_call(input)?;
    Ok((input, Value::FnCall(fn_call)))
}

fn parse_ident_value(input: &str) -> ParseRes<&str, Value> {
    let (input, ident) = parse_ident(input)?;
    Ok((input, Value::Ident(ident)))
}

fn parse_literal_value(input: &str) -> ParseRes<&str, Value> {
    let (input, literal) = parse_literal(input)?;
    Ok((input, Value::Literal(literal)))
}

pub fn test() {
    let module = parse_module(
        r#"
    fn main() {
        println(fib(22));
    }

    fn fib(n: num) -> num {
        if n == 0 {
            return 0;
        }

        if n == 1 {
            return 1;
        }

        return fib(n - 1) + fib(n - 2);
    }
    "#,
    )
    .unwrap();

    let mut globals = vec![
        mid_level::Ident("println".into()),
        mid_level::Ident("add".into()),
        mid_level::Ident("sub".into()),
        mid_level::Ident("eq".into()),
    ];

    for fun in &module.1.functions {
        globals.push(ident_to_mid_level(fun.name.clone()));
    }

    for fun in module.1.functions {
        let fun = fn_to_mid_level(fun);

        let ctx = mid_level::gen_fn_bbs(globals.clone(), fun);

        let mut out = String::new();
        codegen::gen_from_context(&mut out, &ctx);

        println!("{out}");
    }
}

pub fn fn_to_mid_level(fun: Function) -> mid_level::Function {
    mid_level::Function {
        name: fun.name.0,
        args: fun
            .args
            .into_iter()
            .map(|a| ident_to_mid_level(a.ident))
            .collect(),
        block: mid_level::FunctionBlock::Regular(block_to_mid_level(fun.block)),
    }
}

pub fn ident_to_mid_level(ident: Ident) -> mid_level::Ident {
    mid_level::Ident(ident.0)
}

pub fn stmt_to_mid_level(stmt: Statement) -> Vec<mid_level::Statement> {
    match stmt {
        Statement::Assign(RawAssign { target, value }) => {
            vec![mid_level::Statement::VarAssign(
                ident_to_mid_level(target),
                expr_to_mid_level(value),
            )]
        }
        Statement::Block(b) => b
            .statements
            .into_iter()
            .flat_map(stmt_to_mid_level)
            .collect(),
        Statement::Def(RawDef { new_var, value }) => vec![mid_level::Statement::VarDecl(
            ident_to_mid_level(new_var),
            Some(expr_to_mid_level(value)),
        )],
        Statement::Expression(e) => {
            // FIXME: This is a hacky way of returning the last expression
            vec![mid_level::Statement::VarAssign(
                RETURN_ARG_IDENT.clone(),
                expr_to_mid_level(e),
            )]
        }
        Statement::If(RawIf {
            condition,
            then_block,
            else_ifs,
            else_block,
        }) => {
            if !else_ifs.is_empty() {
                todo!()
            }

            vec![mid_level::Statement::If(mid_level::IfStatement {
                condition: expr_to_mid_level(condition),
                success: block_to_mid_level(then_block),
                failure: else_block.map(|b| block_to_mid_level(b)),
            })]
        }
        Statement::Return(RawReturn { to_return }) => {
            vec![mid_level::Statement::Return(Some(expr_to_mid_level(
                to_return,
            )))]
        }
        Statement::While(RawWhile {
            condition,
            while_block,
        }) => {
            let mut stmts = vec![mid_level::Statement::If(mid_level::IfStatement {
                condition: expr_to_mid_level(condition),
                success: mid_level::Block { stmts: vec![] },
                failure: Some(mid_level::Block {
                    stmts: vec![mid_level::Statement::Break],
                }),
            })];
            stmts.append(&mut block_to_mid_level(while_block).stmts);

            vec![mid_level::Statement::Loop(mid_level::Block { stmts })]
        }
    }
}

pub fn expr_to_mid_level(expr: Expr) -> mid_level::Expr {
    match expr {
        Expr::EBin(BinExpr { lhs, rhs, op }) => {
            let op = match op {
                BinOp::Add => "add",
                BinOp::Eq => "eq",
                BinOp::Sub => "sub",
                _ => todo!("{op:?}"),
            };
            mid_level::Expr::FnCall(mid_level::FnCallExpr {
                callable: Box::new(mid_level::Expr::Global(mid_level::Ident(op.into()))),
                args: vec![expr_to_mid_level(*lhs), expr_to_mid_level(*rhs)],
            })
        }

        Expr::EVal(v) => match *v {
            Value::FnCall(f) => mid_level::Expr::FnCall(mid_level::FnCallExpr {
                callable: Box::new(mid_level::Expr::Global(ident_to_mid_level(f.name))),
                args: f.args.into_iter().map(expr_to_mid_level).collect(),
            }),
            Value::Ident(i) => mid_level::Expr::Local(ident_to_mid_level(i)),
            Value::Literal(Literal::Number(n)) => {
                mid_level::Expr::Immediate(mid_level::Immediate { val: n as u64 })
            }
            _ => todo!(),
        },
    }
}

pub fn block_to_mid_level(b: Block) -> mid_level::Block {
    mid_level::Block {
        stmts: b
            .statements
            .into_iter()
            .flat_map(stmt_to_mid_level)
            .collect(),
    }
}
