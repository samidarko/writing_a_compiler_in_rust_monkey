use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use monkey_interpreter_rs::{
    evaluator, lexer::Lexer, object::environment::Environment, parser::Parser,
};

fn benchmark_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");
    
    let simple_code = "let x = 5;";
    let complex_code = r#"
        let fibonacci = fn(x) {
            if (x < 2) {
                return x;
            } else {
                return fibonacci(x - 1) + fibonacci(x - 2);
            }
        };
        fibonacci(10);
    "#;
    let array_code = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
    let hash_code = r#"{"key1": "value1", "key2": "value2", "key3": "value3"}"#;

    group.bench_with_input(BenchmarkId::new("simple", "let_assignment"), &simple_code, |b, code| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(code.chars().collect()));
            while lexer.next_token().is_ok() {
                // Consume all tokens
            }
        })
    });

    group.bench_with_input(BenchmarkId::new("complex", "fibonacci_function"), &complex_code, |b, code| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(code.chars().collect()));
            while lexer.next_token().is_ok() {
                // Consume all tokens
            }
        })
    });

    group.bench_with_input(BenchmarkId::new("array", "array_literal"), &array_code, |b, code| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(code.chars().collect()));
            while lexer.next_token().is_ok() {
                // Consume all tokens
            }
        })
    });

    group.bench_with_input(BenchmarkId::new("hash", "hash_literal"), &hash_code, |b, code| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(code.chars().collect()));
            while lexer.next_token().is_ok() {
                // Consume all tokens
            }
        })
    });

    group.finish();
}

fn benchmark_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    let simple_code = "let x = 5;";
    let complex_code = r#"
        let fibonacci = fn(x) {
            if (x < 2) {
                return x;
            } else {
                return fibonacci(x - 1) + fibonacci(x - 2);
            }
        };
        fibonacci(10);
    "#;
    let expression_code = "1 + 2 * 3 - 4 / 5 + (6 * 7) - 8";
    let array_code = "[1, 2 * 3, 4 + 5, fn(x) { x + 1 }(6)]";

    group.bench_with_input(BenchmarkId::new("simple", "let_assignment"), &simple_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            parser.parse().unwrap()
        })
    });

    group.bench_with_input(BenchmarkId::new("complex", "fibonacci_function"), &complex_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            parser.parse().unwrap()
        })
    });

    group.bench_with_input(BenchmarkId::new("expression", "arithmetic_expression"), &expression_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            parser.parse().unwrap()
        })
    });

    group.bench_with_input(BenchmarkId::new("array", "complex_array"), &array_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            parser.parse().unwrap()
        })
    });

    group.finish();
}

fn benchmark_evaluator(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluator");

    let simple_code = "let x = 5; x + 10";
    let fibonacci_code = r#"
        let fibonacci = fn(x) {
            if (x < 2) {
                return x;
            } else {
                return fibonacci(x - 1) + fibonacci(x - 2);
            }
        };
        fibonacci(8);
    "#;
    let array_code = r#"
        let arr = [1, 2, 3, 4, 5];
        let sum = fn(arr, i, acc) {
            if (i >= len(arr)) {
                return acc;
            } else {
                return sum(arr, i + 1, acc + arr[i]);
            }
        };
        sum(arr, 0, 0);
    "#;
    let hash_code = r#"
        let hash = {"a": 1, "b": 2, "c": 3};
        hash["a"] + hash["b"] + hash["c"];
    "#;

    group.bench_with_input(BenchmarkId::new("simple", "arithmetic"), &simple_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse().unwrap();
            let env = Environment::new();
            evaluator::eval_program(program, env).unwrap()
        })
    });

    group.bench_with_input(BenchmarkId::new("fibonacci", "recursive_function"), &fibonacci_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse().unwrap();
            let env = Environment::new();
            evaluator::eval_program(program, env).unwrap()
        })
    });

    group.bench_with_input(BenchmarkId::new("array", "array_operations"), &array_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse().unwrap();
            let env = Environment::new();
            evaluator::eval_program(program, env).unwrap()
        })
    });

    group.bench_with_input(BenchmarkId::new("hash", "hash_operations"), &hash_code, |b, code| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(code.chars().collect()));
            let mut parser = Parser::new(lexer).unwrap();
            let program = parser.parse().unwrap();
            let env = Environment::new();
            evaluator::eval_program(program, env).unwrap()
        })
    });

    group.finish();
}

fn benchmark_full_pipeline(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_pipeline");

    let programs = vec![
        ("simple", "let x = 5; let y = 10; x + y"),
        ("moderate", r#"
            let add = fn(a, b) { a + b };
            let multiply = fn(a, b) { a * b };
            add(multiply(2, 3), 4)
        "#),
        ("complex", r#"
            let map = fn(arr, f) {
                let iter = fn(arr, accumulated) {
                    if (len(arr) == 0) {
                        return accumulated;
                    } else {
                        return iter(rest(arr), push(accumulated, f(first(arr))));
                    }
                };
                iter(arr, []);
            };
            let double = fn(x) { x * 2 };
            map([1, 2, 3, 4], double);
        "#),
    ];

    for (name, code) in programs {
        group.bench_with_input(BenchmarkId::new("lex_parse_eval", name), &code, |b, code| {
            b.iter(|| {
                let lexer = Lexer::new(black_box(code.chars().collect()));
                let mut parser = Parser::new(lexer).unwrap();
                let program = parser.parse().unwrap();
                let env = Environment::new();
                evaluator::eval_program(program, env).unwrap()
            })
        });
    }

    group.finish();
}

criterion_group!(benches, benchmark_lexer, benchmark_parser, benchmark_evaluator, benchmark_full_pipeline);
criterion_main!(benches);