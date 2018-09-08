use std::fmt::Display;

use lamcal::{
    parse, ApplicativeOrder, CallByName, CallByValue, Enumerate, HeadSpine, HybridApplicativeOrder,
    HybridNormalOrder, NormalOrder, Prime, Term,
};

use context::Context;
use model::{AlphaRenamingStrategy, BetaReductionStrategy};

pub trait Command {
    type Input;
    type Output: Display;

    fn with_input(input: Self::Input) -> Self;

    fn execute(self, ctx: &mut Context) -> Continuation<Self::Output>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Processing {
    Continue,
    Stop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Continuation<T> {
    pub processing: Processing,
    pub output: Option<T>,
    pub info: Option<String>,
    pub warning: Option<String>,
    pub error: Option<String>,
    pub prompt: String,
}

pub fn cont_output<T>(output: T, prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Continue,
        output: Some(output),
        info: None,
        warning: None,
        error: None,
        prompt: prompt.to_string(),
    }
}

pub fn cont_next<T>(prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Continue,
        output: None,
        info: None,
        warning: None,
        error: None,
        prompt: prompt.to_string(),
    }
}

pub fn cont_info<T>(info: impl Display, prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Continue,
        output: None,
        info: Some(info.to_string()),
        warning: None,
        error: None,
        prompt: prompt.to_string(),
    }
}

pub fn cont_warn<T>(warning: impl Display, prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Continue,
        output: None,
        info: None,
        warning: Some(warning.to_string()),
        error: None,
        prompt: prompt.to_string(),
    }
}

pub fn cont_err<T>(error: impl Display, prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Continue,
        output: None,
        info: None,
        warning: None,
        error: Some(error.to_string()),
        prompt: prompt.to_string(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PrintAlphaRenamingStrategy;

impl Command for PrintAlphaRenamingStrategy {
    type Input = ();
    type Output = String;

    fn with_input(_input: <Self as Command>::Input) -> Self {
        PrintAlphaRenamingStrategy
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        cont_output(
            format!("α-conversion strategy = {}", ctx.alpha_renaming_strategy()),
            "",
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SetAlphaRenamingStrategy {
    input: AlphaRenamingStrategy,
}

impl Command for SetAlphaRenamingStrategy {
    type Input = AlphaRenamingStrategy;
    type Output = String;

    fn with_input(input: <Self as Command>::Input) -> Self {
        SetAlphaRenamingStrategy { input }
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        ctx.set_alpha_renaming_strategy(self.input);
        cont_output(
            format!(
                "Set α-conversion strategy to `{}`",
                ctx.alpha_renaming_strategy()
            ),
            "",
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PrintBetaReductionStrategy;

impl Command for PrintBetaReductionStrategy {
    type Input = ();
    type Output = String;

    fn with_input(_input: <Self as Command>::Input) -> Self {
        PrintBetaReductionStrategy
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        cont_output(
            format!("β-reduction strategy = {}", ctx.beta_reduction_strategy()),
            "",
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SetBetaReductionStrategy {
    input: BetaReductionStrategy,
}

impl Command for SetBetaReductionStrategy {
    type Input = BetaReductionStrategy;
    type Output = String;

    fn with_input(input: <Self as Command>::Input) -> Self {
        SetBetaReductionStrategy { input }
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        ctx.set_beta_reduction_strategy(self.input);
        cont_output(
            format!(
                "Set β-reduction strategy to `{}`",
                ctx.beta_reduction_strategy()
            ),
            "",
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseLambdaExpression<'a> {
    input: &'a str,
}

impl<'a> Command for ParseLambdaExpression<'a> {
    type Input = &'a str;
    type Output = Term;

    fn with_input(input: <Self as Command>::Input) -> Self {
        ParseLambdaExpression { input }
    }

    fn execute(self, _ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        match parse(self.input.chars()) {
            Ok(expr) => cont_output(expr, ""),
            Err(err) => cont_err(err, ""),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpandLambdaExpression<'a> {
    input: &'a str,
}

impl<'a> Command for ExpandLambdaExpression<'a> {
    type Input = &'a str;
    type Output = Term;

    fn with_input(input: <Self as Command>::Input) -> Self {
        ExpandLambdaExpression { input }
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        match parse(self.input.chars()) {
            Ok(mut expr) => {
                expr.expand(ctx.env());
                cont_output(expr, "")
            },
            Err(err) => cont_err(err, ""),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BetaReduceLambdaExpression<'a> {
    input: &'a str,
}

impl<'a> Command for BetaReduceLambdaExpression<'a> {
    type Input = &'a str;
    type Output = Term;

    fn with_input(input: <Self as Command>::Input) -> Self {
        BetaReduceLambdaExpression { input }
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        match parse(self.input.chars()) {
            Ok(mut expr) => {
                match ctx.alpha_renaming_strategy() {
                    AlphaRenamingStrategy::Enumerate => match ctx.beta_reduction_strategy() {
                        BetaReductionStrategy::ApplicativeOrder => {
                            expr.reduce::<ApplicativeOrder<Enumerate>>()
                        },
                        BetaReductionStrategy::CallByName => expr.reduce::<CallByName<Enumerate>>(),
                        BetaReductionStrategy::CallByValue => {
                            expr.reduce::<CallByValue<Enumerate>>()
                        },
                        BetaReductionStrategy::HeadSpine => expr.reduce::<HeadSpine<Enumerate>>(),
                        BetaReductionStrategy::HybridApplicativeOrder => {
                            expr.reduce::<HybridApplicativeOrder<Enumerate>>()
                        },
                        BetaReductionStrategy::HybridNormalOrder => {
                            expr.reduce::<HybridNormalOrder<Enumerate>>()
                        },
                        BetaReductionStrategy::NormalOrder => {
                            expr.reduce::<NormalOrder<Enumerate>>()
                        },
                    },
                    AlphaRenamingStrategy::Prime => match ctx.beta_reduction_strategy() {
                        BetaReductionStrategy::ApplicativeOrder => {
                            expr.reduce::<ApplicativeOrder<Prime>>()
                        },
                        BetaReductionStrategy::CallByName => expr.reduce::<CallByName<Prime>>(),
                        BetaReductionStrategy::CallByValue => expr.reduce::<CallByValue<Prime>>(),
                        BetaReductionStrategy::HeadSpine => expr.reduce::<HeadSpine<Prime>>(),
                        BetaReductionStrategy::HybridApplicativeOrder => {
                            expr.reduce::<HybridApplicativeOrder<Prime>>()
                        },
                        BetaReductionStrategy::HybridNormalOrder => {
                            expr.reduce::<HybridNormalOrder<Prime>>()
                        },
                        BetaReductionStrategy::NormalOrder => expr.reduce::<NormalOrder<Prime>>(),
                    },
                }
                cont_output(expr, "")
            },
            Err(err) => cont_err(err, ""),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvaluateLambdaExpression<'a> {
    input: &'a str,
}

impl<'a> Command for EvaluateLambdaExpression<'a> {
    type Input = &'a str;
    type Output = Term;

    fn with_input(input: <Self as Command>::Input) -> Self {
        EvaluateLambdaExpression { input }
    }

    fn execute(self, ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        match parse(self.input.chars()) {
            Ok(mut expr) => {
                match ctx.alpha_renaming_strategy() {
                    AlphaRenamingStrategy::Enumerate => match ctx.beta_reduction_strategy() {
                        BetaReductionStrategy::ApplicativeOrder => {
                            expr.evaluate::<ApplicativeOrder<Enumerate>>(ctx.env())
                        },
                        BetaReductionStrategy::CallByName => {
                            expr.evaluate::<CallByName<Enumerate>>(ctx.env())
                        },
                        BetaReductionStrategy::CallByValue => {
                            expr.evaluate::<CallByValue<Enumerate>>(ctx.env())
                        },
                        BetaReductionStrategy::HeadSpine => {
                            expr.evaluate::<HeadSpine<Enumerate>>(ctx.env())
                        },
                        BetaReductionStrategy::HybridApplicativeOrder => {
                            expr.evaluate::<HybridApplicativeOrder<Enumerate>>(ctx.env())
                        },
                        BetaReductionStrategy::HybridNormalOrder => {
                            expr.evaluate::<HybridNormalOrder<Enumerate>>(ctx.env())
                        },
                        BetaReductionStrategy::NormalOrder => {
                            expr.evaluate::<NormalOrder<Enumerate>>(ctx.env())
                        },
                    },
                    AlphaRenamingStrategy::Prime => match ctx.beta_reduction_strategy() {
                        BetaReductionStrategy::ApplicativeOrder => {
                            expr.evaluate::<ApplicativeOrder<Prime>>(ctx.env())
                        },
                        BetaReductionStrategy::CallByName => {
                            expr.evaluate::<CallByName<Prime>>(ctx.env())
                        },
                        BetaReductionStrategy::CallByValue => {
                            expr.evaluate::<CallByValue<Prime>>(ctx.env())
                        },
                        BetaReductionStrategy::HeadSpine => {
                            expr.evaluate::<HeadSpine<Prime>>(ctx.env())
                        },
                        BetaReductionStrategy::HybridApplicativeOrder => {
                            expr.evaluate::<HybridApplicativeOrder<Prime>>(ctx.env())
                        },
                        BetaReductionStrategy::HybridNormalOrder => {
                            expr.evaluate::<HybridNormalOrder<Prime>>(ctx.env())
                        },
                        BetaReductionStrategy::NormalOrder => {
                            expr.evaluate::<NormalOrder<Prime>>(ctx.env())
                        },
                    },
                }
                cont_output(expr, "")
            },
            Err(err) => cont_err(err, ""),
        }
    }
}
