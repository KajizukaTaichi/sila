use sila_transpiler_infrastructure::{
    transpile_javascript, Block, Expr, Instruction, Operator, Type,
};

fn main() {
    let program: Block = vec![
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
                Instruction::Print(Expr::Expr(vec![
                    Expr::Literal(Type::String("i: ".to_string())),
                    Expr::Operator(Operator::Add),
                    Expr::Variable("i".to_string()),
                ])),
            ],
        ),
    ];
    println!("{}", transpile_javascript(program));
}
