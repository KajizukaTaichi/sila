use sila_transpiler_infrastructure::{
    transpile_javascript, transpile_ruby, Block, Expr, Instruction, Operator, Type,
};

fn main() {
    let program: Block = vec![
        Instruction::Function(
            "show".to_string(),
            vec!["i".to_string()],
            vec![Instruction::Return(Some(Expr::Expr(vec![
                Expr::Literal(Type::String("counter value is ".to_string())),
                Expr::Operator(Operator::Add),
                Expr::Call("String".to_string(), vec![Expr::Variable("i".to_string())]),
            ])))],
        ),
        Instruction::Let("i".to_string(), Expr::Literal(Type::Integer(0))),
        Instruction::While(
            Expr::Expr(vec![
                Expr::Variable("i".to_string()),
                Expr::Operator(Operator::Less),
                Expr::Literal(Type::Integer(10)),
            ]),
            vec![
                Instruction::Variable(
                    "i".to_string(),
                    Expr::Expr(vec![
                        Expr::Variable("i".to_string()),
                        Expr::Operator(Operator::Add),
                        Expr::Literal(Type::Integer(1)),
                    ]),
                ),
                Instruction::Print(Expr::Call(
                    "show".to_string(),
                    vec![Expr::Variable("i".to_string())],
                )),
            ],
        ),
    ];
    println!("{}", transpile_javascript(program.clone()));
    println!("{}", transpile_ruby(program));
}
