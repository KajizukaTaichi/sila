use sila_transpiler_infrastructure::{
    transpile_javascript, transpile_python, transpile_ruby, Block, Expr, Instruction, Library,
    Operator, Type,
};

fn main() {
    let program: Block = vec![
        Instruction::Function(
            "show".to_string(),
            vec!["count".to_string()],
            vec![Instruction::Return(Some(Expr::Expr(vec![
                Expr::Literal(Type::String("counter value is ".to_string())),
                Expr::Operator(Operator::Add),
                Expr::Library(Library::ToString, vec![Expr::Variable("count".to_string())]),
            ])))],
        ),
        Instruction::Let("count".to_string(), Expr::Literal(Type::Integer(0))),
        Instruction::TryError(
            vec![Instruction::Let(
                "limit".to_string(),
                Expr::Library(
                    Library::ToInterger,
                    vec![Expr::Library(
                        Library::Input,
                        vec![Expr::Literal(Type::String("limit: ".to_string()))],
                    )],
                ),
            )],
            vec![Instruction::Let(
                "limit".to_string(),
                Expr::Literal(Type::Integer(10)),
            )],
        ),
        Instruction::While(
            Expr::Expr(vec![
                Expr::Variable("count".to_string()),
                Expr::Operator(Operator::Less),
                Expr::Variable("limit".to_string()),
            ]),
            vec![
                Instruction::Variable(
                    "count".to_string(),
                    Expr::Expr(vec![
                        Expr::Variable("count".to_string()),
                        Expr::Operator(Operator::Add),
                        Expr::Literal(Type::Integer(1)),
                    ]),
                ),
                Instruction::If(
                    Expr::Expr(vec![
                        Expr::Variable("count".to_string()),
                        Expr::Operator(Operator::Mod),
                        Expr::Literal(Type::Integer(2)),
                        Expr::Operator(Operator::Equal),
                        Expr::Literal(Type::Integer(0)),
                    ]),
                    vec![Instruction::Continue],
                    None,
                ),
                Instruction::Print(Expr::Call(
                    "show".to_string(),
                    vec![Expr::Variable("count".to_string())],
                )),
            ],
        ),
    ];
    println!("JavaScript:\n{}\n", transpile_javascript(program.clone()));
    println!("Ruby:\n{}\n", transpile_ruby(program.clone()));
    println!("Python:\n{}\n", transpile_python(program));
}
