use std::collections::HashMap;

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
    Println(Expr),
    Let(String, Expr),
    If(Expr, Block, Block),
    While(Expr, Block),
    Variable(String),
}

impl Instruction {
    fn codegen(&self, platform: Platform) -> String {
        match platform.clone() {
            Platform::JavaScript => match self {
                Instruction::Print(expr) => format!("console.log({})", expr.codegen(platform)),
                _ => panic!("Unknown instruction"),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Expr(Vec<Expr>),
    Operator(Operator),
    Literal(Type),
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
            }
            .to_string(),
        }
    }
}

fn codegen_block(program: Block, platform: Platform) -> String {
    program
        .iter()
        .map(|x| x.codegen(platform.clone()))
        .collect::<Vec<String>>()
        .join(match platform {
            Platform::JavaScript => ";",
        })
}

fn main() {
    let program = vec![Instruction::Print(Expr::Expr(vec![
        Expr::Literal(Type::Integer(5)),
        Expr::Operator(Operator::Add),
        Expr::Literal(Type::Integer(5)),
    ]))];
    println!("{}", codegen_block(program, Platform::JavaScript));
}
