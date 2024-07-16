use std::{collections::HashMap, fmt::format};

#[derive(Debug, Clone)]
enum Platform {
    JavaScript,
}

#[derive(Debug, Clone)]
enum Type {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Array(Vec<Type>),
    Dict(HashMap<Type, Type>),
}

impl Type {
    fn codegen(&self, platform: Platform) -> String {
        match platform.clone() {
            Platform::JavaScript => format!(
                "{}",
                match self.clone() {
                    Type::Integer(i) => i.to_string(),
                    Type::Float(f) => f.to_string(),
                    Type::String(s) => format!("`{s}`"),
                    Type::Array(a) => format!(
                        "[{}]",
                        a.iter()
                            .map(|i| i.clone().codegen(platform.clone()))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    Type::Bool(b) => b.to_string(),
                    Type::Dict(d) => format!(
                        "{{{}}}",
                        d.iter()
                            .map(|(k, v)| {
                                format!(
                                    "{}:{}",
                                    k.codegen(platform.clone()),
                                    v.codegen(platform.clone())
                                )
                            })
                            .collect::<Vec<String>>()
                            .join(",\n")
                    ),
                }
            ),
        }
    }
}

type Block = Vec<Instruction>;

#[derive(Debug, Clone)]
enum Instruction {
    Print(Expr),
    Let(String, Expr),
    If(Expr, Block, Block),
    While(Expr, Block),
}

impl Instruction {
    fn codegen(&self, platform: Platform) -> String {
        match platform.clone() {
            Platform::JavaScript => match self {
                Instruction::Print(expr) => format!("console.log({})", expr.codegen(platform)),
                Instruction::Let(name, value) => {
                    format!("let {name} = {}", value.codegen(platform))
                }
                Instruction::If(condition, true_block, false_block) => format!(
                    "if {} {} else {}",
                    condition.codegen(platform.clone()),
                    codegen_block(true_block.clone(), platform.clone()),
                    codegen_block(false_block.clone(), platform)
                ),
                Instruction::While(condition, code_block) => format!(
                    "while {} {}",
                    condition.codegen(platform.clone()),
                    codegen_block(code_block.clone(), platform.clone()),
                ),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Expr(Vec<Expr>),
    Operator(Operator),
    Literal(Type),
    Variable(String),
}

impl Expr {
    fn codegen(&self, platform: Platform) -> String {
        match platform.clone() {
            Platform::JavaScript => match self {
                Expr::Expr(exprs) => format!(
                    "({})",
                    exprs
                        .iter()
                        .map(|i| i.codegen(platform.clone()))
                        .collect::<Vec<String>>()
                        .join(" ")
                ),
                Expr::Literal(value) => value.codegen(platform.clone()),
                Expr::Operator(o) => o.codegen(platform.clone()),
                Expr::Variable(v) => v.clone(),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Equal,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
    Not,
}

impl Operator {
    fn codegen(&self, platform: Platform) -> String {
        match platform {
            Platform::JavaScript => match self {
                Operator::Add => "+",
                Operator::Sub => "-",
                Operator::Mul => "*",
                Operator::Div => "/",
                Operator::Pow => "**",
                Operator::Mod => "%",
                Operator::Equal => "==",
                Operator::NotEq => "!=",
                Operator::Less => "<",
                Operator::LessEq => "<=",
                Operator::Greater => ">",
                Operator::GreaterEq => ">=",
                Operator::And => "&&",
                Operator::Or => "||",
                Operator::Not => "!",
            }
            .to_string(),
        }
    }
}

fn codegen_block(program: Block, platform: Platform) -> String {
    match platform {
        Platform::JavaScript => format!(
            "{{\n{}\n}}",
            program
                .iter()
                .map(|x| x.codegen(platform.clone()))
                .collect::<Vec<String>>()
                .join(";\n")
        ),
    }
}

fn main() {
    let program: Block = vec![
        Instruction::Let(
            "i".to_string(),
            Expr::Expr(vec![Expr::Literal(Type::Integer(0))]),
        ),
        Instruction::While(
            Expr::Expr(vec![Expr::Literal(Type::Integer(0))]),
            Expr::Expr(vec![Expr::Literal(Type::Integer(0))]),
        ),
    ];
    println!("{}", codegen_block(program, Platform::JavaScript));
}
