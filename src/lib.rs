use std::collections::HashMap;

/// Supported platform to transpile
#[derive(Debug, Clone)]
enum Platform {
    JavaScript,
    Ruby,
    Python,
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
    /// Symbol such as key of the hashmap, object and dictionary, etc
    Symbol(String),
}

impl Type {
    /// Generate the transpliled code
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
                    Type::Symbol(s) => s,
                }
            ),
            Platform::Ruby => format!(
                "{}",
                match self.clone() {
                    Type::Integer(i) => i.to_string(),
                    Type::Float(f) => f.to_string(),
                    Type::String(s) => format!("\"{s}\""),
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
                            .join(", ")
                    ),
                    Type::Symbol(s) => s,
                }
            ),
            Platform::Python => format!(
                "{}",
                match self.clone() {
                    Type::Integer(i) => i.to_string(),
                    Type::Float(f) => f.to_string(),
                    Type::String(s) => format!("'{s}'"),
                    Type::Array(a) => format!(
                        "[{}]",
                        a.iter()
                            .map(|i| i.clone().codegen(platform.clone()))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    Type::Bool(b) => if b { "True" } else { "False" }.to_string(),
                    Type::Dict(d) => format!(
                        "{{{}}}",
                        d.iter()
                            .map(|(k, v)| {
                                format!(
                                    "'{}': {}",
                                    k.codegen(platform.clone()),
                                    v.codegen(platform.clone())
                                )
                            })
                            .collect::<Vec<String>>()
                            .join(",")
                    ),
                    Type::Symbol(s) => s,
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
    /// Standard output
    Print(Expr),
    /// Define variable
    Let(String, Expr),
    /// Define constance
    Const(String, Expr),
    /// Change variable's data
    Variable(String, Expr),
    /// If-else conditional branch
    If(Expr, Block, Option<Block>),
    /// While loop
    While(Expr, Block),
    /// Break the loop
    Break,
    /// Continue the loop
    Continue,
    /// Define function
    Function(String, Vec<String>, Block),
    /// Return value
    Return(Option<Expr>),
    /// Code comment
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
                Instruction::If(condition, true_block, false_block) => {
                    if let Some(false_block) = false_block {
                        format!(
                            "if {} {{\n{}\n}} else {{\n{}\n}}",
                            condition.codegen(platform.clone()),
                            codegen_block(true_block.clone(), platform.clone(), true),
                            codegen_block(false_block.clone(), platform, true)
                        )
                    } else {
                        format!(
                            "if {} {{\n{}\n}}",
                            condition.codegen(platform.clone()),
                            codegen_block(true_block.clone(), platform.clone(), true),
                        )
                    }
                }
                Instruction::While(condition, code_block) => format!(
                    "while {} {{\n{}\n}}",
                    condition.codegen(platform.clone()),
                    codegen_block(code_block.clone(), platform.clone(), true),
                ),
                Instruction::Break => "break".to_string(),
                Instruction::Continue => "continue".to_string(),
                Instruction::Function(name, args, code_block) => format!(
                    "function {name}({}) {{\n{}\n}}",
                    args.join(", "),
                    codegen_block(code_block.clone(), platform.clone(), true),
                ),
                Instruction::Return(v) => {
                    if v.clone().is_some() {
                        format!("return {}", v.clone().unwrap().codegen(platform.clone()))
                    } else {
                        "return".to_string()
                    }
                }
                Instruction::Comment(data) => {
                    if data.contains("\n") {
                        format!("/* {data} */")
                    } else {
                        format!("// {data}")
                    }
                }
            },
            Platform::Ruby => match self {
                Instruction::Print(expr) => format!("puts {}", expr.codegen(platform)),
                Instruction::Let(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
                }
                Instruction::Const(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
                }
                Instruction::Variable(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
                }
                Instruction::If(condition, true_block, false_block) => {
                    if let Some(false_block) = false_block {
                        format!(
                            "if {}\n{}\nelse\n{}\nend",
                            condition.codegen(platform.clone()),
                            codegen_block(true_block.clone(), platform.clone(), true),
                            codegen_block(false_block.clone(), platform, true)
                        )
                    } else {
                        format!(
                            "if {}\n{}\nend",
                            condition.codegen(platform.clone()),
                            codegen_block(true_block.clone(), platform.clone(), true),
                        )
                    }
                }

                Instruction::While(condition, code_block) => format!(
                    "while {} do\n{}\nend",
                    condition.codegen(platform.clone()),
                    codegen_block(code_block.clone(), platform.clone(), true),
                ),
                Instruction::Break => "break".to_string(),
                Instruction::Continue => "next".to_string(),
                Instruction::Function(name, args, code_block) => format!(
                    "def {name}({})\n{}\nend",
                    args.join(", "),
                    codegen_block(code_block.clone(), platform.clone(), true),
                ),
                Instruction::Return(v) => {
                    if v.clone().is_some() {
                        format!("return {}", v.clone().unwrap().codegen(platform.clone()))
                    } else {
                        "return".to_string()
                    }
                }
                Instruction::Comment(data) => {
                    if data.contains("\n") {
                        format!("=begin\n{data}\n=end")
                    } else {
                        format!("# {data}")
                    }
                }
            },
            Platform::Python => match self {
                Instruction::Print(expr) => format!("print({})", expr.codegen(platform)),
                Instruction::Let(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
                }
                Instruction::Const(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
                }
                Instruction::Variable(name, value) => {
                    format!("{name} = {}", value.codegen(platform))
                }
                Instruction::If(condition, true_block, false_block) => {
                    if let Some(false_block) = false_block {
                        format!(
                            "if {}:\n{}\nelse:\n{}\n",
                            condition.codegen(platform.clone()),
                            codegen_block(true_block.clone(), platform.clone(), true),
                            codegen_block(false_block.clone(), platform, true)
                        )
                    } else {
                        format!(
                            "if {}:\n{}\n",
                            condition.codegen(platform.clone()),
                            codegen_block(true_block.clone(), platform.clone(), true),
                        )
                    }
                }

                Instruction::While(condition, code_block) => format!(
                    "while {}:\n{}\n",
                    condition.codegen(platform.clone()),
                    codegen_block(code_block.clone(), platform.clone(), true),
                ),
                Instruction::Break => "break".to_string(),
                Instruction::Continue => "continue".to_string(),
                Instruction::Function(name, args, code_block) => format!(
                    "def {name}({}):\n{}\n",
                    args.join(", "),
                    codegen_block(code_block.clone(), platform.clone(), true),
                ),
                Instruction::Return(v) => {
                    if v.clone().is_some() {
                        format!("return {}", v.clone().unwrap().codegen(platform.clone()))
                    } else {
                        "return".to_string()
                    }
                }
                Instruction::Comment(data) => {
                    if data.contains("\n") {
                        format!("\"\"\"\n{data}\n\"\"\"")
                    } else {
                        format!("# {data}")
                    }
                }
            },
        }
    }
}

/// Expression
#[derive(Debug, Clone)]
pub enum Expr {
    /// Expression in the brackets
    Expr(Vec<Expr>),
    /// Operator of the expression
    Operator(Operator),
    /// Literal data
    Literal(Type),
    /// Variable reference
    Variable(String),
    /// Call library function
    Call(String, Vec<Expr>),
}

impl Expr {
    /// Generate the transpliled code
    fn codegen(&self, platform: Platform) -> String {
        match platform.clone() {
            Platform::JavaScript | Platform::Ruby | Platform::Python => match self {
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
                Expr::Call(identify, args) => format!(
                    "{identify}({})",
                    args.iter()
                        .map(|i| i.codegen(platform.clone()))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
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
            Platform::JavaScript | Platform::Ruby => match self {
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
            },
            Platform::Python => match self {
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
                Operator::And => "and",
                Operator::Or => "or",
                Operator::Not => "not",
            },
        }
        .to_string()
    }
}

/// generate transpiled code of blocked
fn codegen_block(program: Block, platform: Platform, indent: bool) -> String {
    match platform {
        Platform::JavaScript => format!(
            "{}",
            program
                .iter()
                .map(|x| x.codegen(platform.clone()))
                .collect::<Vec<String>>()
                .join(";\n")
                .split("\n")
                .collect::<Vec<&str>>()
                .iter()
                .map(|x| if indent {
                    "    ".to_string() + &x
                } else {
                    x.to_string()
                })
                .collect::<Vec<String>>()
                .join("\n")
        ),
        Platform::Ruby | Platform::Python => format!(
            "{}",
            program
                .iter()
                .map(|x| x.codegen(platform.clone()))
                .collect::<Vec<String>>()
                .join("\n")
                .split("\n")
                .collect::<Vec<&str>>()
                .iter()
                .map(|x| if indent {
                    "    ".to_string() + &x
                } else {
                    x.to_string()
                })
                .collect::<Vec<String>>()
                .join("\n")
        ),
    }
}

/// Transpile to JavaScript
pub fn transpile_javascript(program: Block) -> String {
    format!(
        "// Sila transpiled this code\n{}",
        codegen_block(program, Platform::JavaScript, false)
    )
}

/// Transpile to Ruby
pub fn transpile_ruby(program: Block) -> String {
    format!(
        "# Sila transpiled this code\n{}",
        codegen_block(program, Platform::Ruby, false)
    )
}

/// Transpile to python
pub fn transpile_python(program: Block) -> String {
    format!(
        "# Sila transpiled this code\n{}",
        codegen_block(program, Platform::Python, false)
    )
}
