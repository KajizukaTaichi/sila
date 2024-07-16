use std::collections::HashMap;

/// Supported platform to transpile
#[derive(Debug, Clone)]
enum Platform {
    JavaScript,
}

/// Deta type
#[derive(Debug, Clone)]
pub enum Type {
    /// 64 bit integer
    Integer(i64),
    /// 64 bit float number
    Float(f64),
    /// String of UTF-8
    String(String),
    /// Bool of true or false
    Bool(bool),
    /// Array doesn't mind about type
    Array(Vec<Type>),
    /// Dictionary or object
    Dict(HashMap<Type, Type>),
}

impl Type {
    // Generate the transpliled code
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

/// Code block
pub type Block = Vec<Instruction>;

/// Program's normal instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    Print(Expr),
    Let(String, Expr),
    Const(String, Expr),
    Variable(String, Expr),
    If(Expr, Block, Block),
    While(Expr, Block),
    Function(String, Vec<String>, Block),
    Call(String, Vec<Expr>),
    Comment(String),
}

impl Instruction {
    /// Generate the transpliled code
    fn codegen(&self, platform: Platform) -> String {
        match platform.clone() {
            Platform::JavaScript => match self {
                Instruction::Print(expr) => format!("console.log({})", expr.codegen(platform)),
                Instruction::Let(name, value) => {
                    format!("let {name} = {}", value.codegen(platform))
                }
                Instruction::Const(name, value) => {
                    format!("const {name} = {}", value.codegen(platform))
                }
                Instruction::Variable(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
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
                Instruction::Function(name, args, code_block) => format!(
                    "function {name}({}) {}",
                    args.join(", "),
                    codegen_block(code_block.clone(), platform.clone()),
                ),
                Instruction::Call(identify, args) => format!(
                    "{identify}({})",
                    args.iter()
                        .map(|i| i.codegen(platform.clone()))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Instruction::Comment(data) => {
                    if data.contains("\n") {
                        format!("/* {data} */")
                    } else {
                        format!("// {data}")
                    }
                }
            },
        }
    }
}

/// Expression
#[derive(Debug, Clone)]
pub enum Expr {
    Expr(Vec<Expr>),
    Operator(Operator),
    Literal(Type),
    Variable(String),
}

impl Expr {
    /// Generate the transpliled code
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

/// Operator of expression
#[derive(Debug, Clone)]
pub enum Operator {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Power
    Pow,
    /// Modulo
    Mod,
    /// Equal
    Equal,
    /// Not equal
    NotEq,
    /// Lessthan
    Less,
    /// Lessthan with equal
    LessEq,
    /// Greaterthan
    Greater,
    /// Greaterthan with equal
    GreaterEq,
    /// Logial and
    And,
    /// Logical or
    Or,
    /// Logial not
    Not,
}

impl Operator {
    /// Generate the transpliled code
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

/// generate transpiled code of blocked
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

/// Transpile to JavaScript
pub fn transpile_javascript(program: Block) -> String {
    format!(
        "// Sila transpiled this code\n{}",
        codegen_block(program, Platform::JavaScript)
    )
}
