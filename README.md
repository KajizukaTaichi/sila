# Sila
The simple transpiler infrastructure for every platform.<br> But, now is support JavaScript, Ruby and Python. Other platforms are coming soon!

## [Example](/examples/countup.rs)
```rust
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
```
### Output
```
JavaScript:
// Sila transpiled this code
function show(count) {
    return (`counter value is ` + String(count))
};
let count = 0;
try {
    let limit = parseInt(prompt(`limit: `))
} catch {
    let limit = 10
};
while (count < limit) {
    count = (count + 1);
    if (count % 2 == 0) {
        continue
    };
    console.log(show(count))
}

Ruby:
# Sila transpiled this code
def input(prompt)
    print prompt
    return gets.chomp
end

def show(count)
    return ("counter value is " + String(count))
end
count = 0
begin
    limit = Integer(input("limit: "))
rescue
    limit = 10
end
while (count < limit) do
    count = (count + 1)
    if (count % 2 == 0)
        next
    end
    puts show(count)
end

Python:
# Sila transpiled this code
def show(count):
    return ('counter value is ' + str(count))
count = 0
try:
    limit = int(input('limit: '))
except:
    limit = 10
while (count < limit):
    count = (count + 1)
    if (count % 2 == 0):
        continue
    print(show(count))
```
