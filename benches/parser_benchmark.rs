#[macro_use]
extern crate criterion;

extern crate lamcal;

use criterion::Criterion;

use lamcal::parser::{parse_tokens, tokenize};
use lamcal::Term;

const EXPRESSION_1: &str = r#"
(\id.id) ((\fun.\first.\second.fun second first) f ((\p.\q.p p q) (\a.\b.a) (\a.\b.b)) \x1.(x1 \x2.x2 x2) x1)
"#;

fn parse_expression1_bench(c: &mut Criterion) {
    c.bench_function("parse expression 1", |b| {
        b.iter(|| {
            parse_tokens(tokenize(EXPRESSION_1.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral0_bench(c: &mut Criterion) {
    let input = Term::from(0usize).to_string();

    c.bench_function("parse numeral 0", move |b| {
        b.iter(|| {
            parse_tokens(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral1100_bench(c: &mut Criterion) {
    let input = Term::from(1100usize).to_string();

    c.bench_function("parse numeral 1100", move |b| {
        b.iter(|| {
            parse_tokens(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

fn parse_numeral4400_bench(c: &mut Criterion) {
    let input = Term::from(4400usize).to_string();

    c.bench_function("parse numeral 4400", move |b| {
        b.iter(|| {
            parse_tokens(tokenize(input.chars()).expect("tokenizable lambda expression"))
                .expect("parsable lambda expression")
        })
    });
}

criterion_group!(
    benches,
    parse_expression1_bench,
    parse_numeral0_bench,
    parse_numeral1100_bench,
    parse_numeral4400_bench,
);
criterion_main!(benches);
