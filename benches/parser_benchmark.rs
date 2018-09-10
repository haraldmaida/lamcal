#[macro_use]
extern crate criterion;

extern crate lamcal;

use criterion::Criterion;

use lamcal::{parse_tokens, parse_tokens_recursive, tokenize, Term};

const EXPRESSION_1: &str = r#"
(\id.id) ((\fun.\first.\second.fun second first) f ((\p.\q.p p q) (\a.\b.a) (\a.\b.b)) \x1.(x1 \x2.x2 x2) x1)
"#;

fn parse_expression1_recursive_bench(c: &mut Criterion) {
    c.bench_function("parse expression 1 recursive", |b| {
        b.iter(|| {
            parse_tokens_recursive(
                tokenize(EXPRESSION_1.chars()).expect("tokenizable lambda expression"),
            ).expect("parsable lambda expression")
        })
    });
}

fn parse_expression1_trampoline_bench(c: &mut Criterion) {
    c.bench_function("parse expression 1 trampoline", |b| {
        b.iter(|| {
            parse_tokens(tokenize(EXPRESSION_1.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral0_recursive_bench(c: &mut Criterion) {
    let input = Term::from(0usize).to_string();

    c.bench_function("parse numeral 0 recursive", move |b| {
        b.iter(|| {
            parse_tokens_recursive(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral0_trampoline_bench(c: &mut Criterion) {
    let input = Term::from(0usize).to_string();

    c.bench_function("parse numeral 0 trampoline", move |b| {
        b.iter(|| {
            parse_tokens(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral1100_recursive_bench(c: &mut Criterion) {
    let input = Term::from(1100usize).to_string();

    c.bench_function("parse numeral 1100 recursive", move |b| {
        b.iter(|| {
            parse_tokens_recursive(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral1100_trampoline_bench(c: &mut Criterion) {
    let input = Term::from(1100usize).to_string();

    c.bench_function("parse numeral 1100 trampoline", move |b| {
        b.iter(|| {
            parse_tokens(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral4400_trampoline_bench(c: &mut Criterion) {
    let input = Term::from(2500usize).to_string();

    c.bench_function("parse numeral 4400 trampoline", move |b| {
        b.iter(|| {
            parse_tokens(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

criterion_group!(
    benches,
    parse_expression1_trampoline_bench,
    parse_expression1_recursive_bench,
    parse_numeral0_trampoline_bench,
    parse_numeral0_recursive_bench,
    parse_numeral1100_trampoline_bench,
    parse_numeral1100_recursive_bench,
    parse_numeral4400_trampoline_bench,
);
criterion_main!(benches);
