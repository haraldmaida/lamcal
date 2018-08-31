//! A command line repl (read-evaluate-print loop) for lambda calculus.

#![doc(html_root_url = "https://docs.rs/lamcal-repl/0.2.1/lamcali")]
#![warn(
    bare_trait_objects,
    missing_copy_implementations,
    missing_debug_implementations,
//    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unstable_features,
    unused_extern_crates,
    unused_import_braces,
    unused_qualifications,
)]

extern crate colored;
extern crate dirs;
#[macro_use]
extern crate failure;
extern crate rustyline;

extern crate lamcal_repl;

mod build {
    pub const NAME: &str = env!("CARGO_PKG_NAME");
    pub const VERSION: &str = env!("CARGO_PKG_VERSION");
}

use std::fmt::Display;
use std::fs;
use std::path::PathBuf;

use colored::*;
use failure::{err_msg, Error};
use rustyline::Editor;

use lamcal_repl::command::{
    cont_output, Command, Continuation, EvaluateLambdaExpression, ParseLambdaExpression,
    PrintAlphaRenamingStrategy, PrintBetaReductionStrategy, Processing, SetAlphaRenamingStrategy,
    SetBetaReductionStrategy,
};
use lamcal_repl::context::Context;
use lamcal_repl::model::{AlphaRenamingStrategy, BetaReductionStrategy};
use lamcal_repl::settings::Settings;

const PROMPT_HEAD: &str = "λ> ";

fn main() {
    print_info(welcome_message());
    let settings = if let Ok(settings_file) = settings_file() {
        match Settings::default().merge(&settings_file) {
            Ok(settings) => settings,
            Err(err) => {
                print_error(err);
                return;
            },
        }
    } else {
        Settings::default()
    };
    let config = rustyline::Config::builder()
        .edit_mode(settings.edit.mode.to_rustyline())
        .history_ignore_dups(settings.history.ignore_dups)
        .history_ignore_space(settings.history.ignore_space)
        .max_history_size(settings.history.max_len)
        .build();
    let mut rl = Editor::<()>::with_config(config);
    match history_file() {
        Ok(history_file) => if let Err(err) = rl.load_history(&history_file) {
            print_warning(err);
        },
        Err(err) => print_warning(err),
    }
    let mut ctx = Context::default();
    let mut prompt = PROMPT_HEAD.to_owned();
    loop {
        match rl.readline(&prompt) {
            Ok(line) => {
                rl.add_history_entry(line.clone());
                let continuation = match parse_command(&line) {
                    Ok(cmd) => cmd.execute(&mut ctx),
                    Err(err) => {
                        print_error(err);
                        next("")
                    },
                };
                prompt = PROMPT_HEAD.to_owned() + &continuation.prompt;
                use self::Processing::*;
                match continuation.processing {
                    Continue => {},
                    Stop => break,
                }
            },
            Err(err) => {
                print_error(err);
                break;
            },
        }
    }
    match history_file() {
        Ok(history_file) => if let Err(err) = rl.save_history(&history_file) {
            print_error(err);
        },
        Err(err) => print_error(err),
    };
}

fn app_dir() -> Result<PathBuf, Error> {
    let mut path = dirs::home_dir()
        .ok_or_else(|| err_msg("The OS environment has no home directory defined"))?;
    path.push(".lamcali");
    if !path.exists() {
        fs::create_dir_all(&path)?;
    }
    Ok(path)
}

fn settings_file() -> Result<PathBuf, Error> {
    app_dir().map(|mut path| {
        path.push("settings.toml");
        path
    })
}

fn history_file() -> Result<PathBuf, Error> {
    app_dir().map(|mut path| {
        path.push("history.txt");
        path
    })
}

fn print(text: impl Display) {
    println!("{}", text)
}

fn print_error(error: impl Display) {
    println!("{} {}", "error:".red(), error)
}

fn print_warning(warning: impl Display) {
    println!("{} {}", "warning:".yellow(), warning)
}

fn print_info(info: impl Display) {
    println!("{} {}", "info:".blue(), info)
}

fn handle_continuation<T>(continuation: Continuation<T>) -> Continuation<()>
where
    T: Display,
{
    if let Some(output) = continuation.output {
        print(output.to_string());
    }
    if let Some(info) = continuation.info {
        print_info(info);
    }
    if let Some(warning) = continuation.warning {
        print_warning(warning);
    }
    if let Some(error) = continuation.error {
        print_error(error);
    }
    match continuation.processing {
        Processing::Continue => next(continuation.prompt),
        Processing::Stop => stop((), continuation.prompt),
    }
}

fn next<T>(prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Continue,
        output: None,
        info: None,
        warning: None,
        error: None,
        prompt: prompt.to_string(),
    }
}

fn stop<T>(output: T, prompt: impl Display) -> Continuation<T> {
    Continuation {
        processing: Processing::Stop,
        output: Some(output),
        info: None,
        warning: None,
        error: None,
        prompt: prompt.to_string(),
    }
}

fn parse_alpha_renaming_strategy(line: &str) -> Result<AlphaRenamingStrategy, Error> {
    use self::AlphaRenamingStrategy::*;
    match line.trim() {
        "enumerate" => Ok(Enumerate),
        "prime" => Ok(Prime),
        s => Err(format_err!("Unknown alpha-reduction strategy: {}", s)),
    }
}

fn parse_beta_reduction_strategy(line: &str) -> Result<BetaReductionStrategy, Error> {
    use self::BetaReductionStrategy::*;
    match line.trim() {
        "app" => Ok(ApplicativeOrder),
        "cbn" => Ok(CallByName),
        "cbv" => Ok(CallByValue),
        "hap" => Ok(HybridApplicativeOrder),
        "hno" => Ok(HybridNormalOrder),
        "hsp" => Ok(HeadSpine),
        "nor" => Ok(NormalOrder),
        s => Err(format_err!("Unknown beta-reduction strategy: {}", s)),
    }
}

enum LciCommand<'a> {
    Help(Help),
    Quit(Quit),
    Version(Version),
    PrintAlphaRenamingStrategy(PrintAlphaRenamingStrategy),
    PrintBetaReductionStrategy(PrintBetaReductionStrategy),
    SetAlphaRenamingStrategy(SetAlphaRenamingStrategy),
    SetBetaReductionStrategy(SetBetaReductionStrategy),
    PrintLambdaExpression(ParseLambdaExpression<'a>),
    EvaluateLambdaExpression(EvaluateLambdaExpression<'a>),
}

impl<'a> LciCommand<'a> {
    fn execute(self, ctx: &mut Context) -> Continuation<()> {
        match self {
            LciCommand::Help(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::Quit(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::Version(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintAlphaRenamingStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintBetaReductionStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::SetAlphaRenamingStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::SetBetaReductionStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintLambdaExpression(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::EvaluateLambdaExpression(cmd) => handle_continuation(cmd.execute(ctx)),
        }
    }
}

impl<'a> From<Help> for LciCommand<'a> {
    fn from(cmd: Help) -> Self {
        LciCommand::Help(cmd)
    }
}

impl<'a> From<Quit> for LciCommand<'a> {
    fn from(cmd: Quit) -> Self {
        LciCommand::Quit(cmd)
    }
}

impl<'a> From<Version> for LciCommand<'a> {
    fn from(cmd: Version) -> Self {
        LciCommand::Version(cmd)
    }
}

impl<'a> From<PrintAlphaRenamingStrategy> for LciCommand<'a> {
    fn from(cmd: PrintAlphaRenamingStrategy) -> Self {
        LciCommand::PrintAlphaRenamingStrategy(cmd)
    }
}

impl<'a> From<PrintBetaReductionStrategy> for LciCommand<'a> {
    fn from(cmd: PrintBetaReductionStrategy) -> Self {
        LciCommand::PrintBetaReductionStrategy(cmd)
    }
}

impl<'a> From<SetAlphaRenamingStrategy> for LciCommand<'a> {
    fn from(cmd: SetAlphaRenamingStrategy) -> Self {
        LciCommand::SetAlphaRenamingStrategy(cmd)
    }
}

impl<'a> From<SetBetaReductionStrategy> for LciCommand<'a> {
    fn from(cmd: SetBetaReductionStrategy) -> Self {
        LciCommand::SetBetaReductionStrategy(cmd)
    }
}

impl<'a> From<ParseLambdaExpression<'a>> for LciCommand<'a> {
    fn from(cmd: ParseLambdaExpression<'a>) -> Self {
        LciCommand::PrintLambdaExpression(cmd)
    }
}

impl<'a> From<EvaluateLambdaExpression<'a>> for LciCommand<'a> {
    fn from(cmd: EvaluateLambdaExpression<'a>) -> Self {
        LciCommand::EvaluateLambdaExpression(cmd)
    }
}

fn parse_command(line: &str) -> Result<LciCommand, Error> {
    match line.trim() {
        ":h" | ":help" => Ok(Help.into()),
        ":q" | ":quit" => Ok(Quit.into()),
        ":v" | ":version" => Ok(Version.into()),
        ":a" | ":alpha" => Ok(PrintAlphaRenamingStrategy.into()),
        ":b" | ":beta" => Ok(PrintBetaReductionStrategy.into()),
        cmd if cmd.starts_with(":a") || cmd.starts_with(":alpha") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                parse_alpha_renaming_strategy(&line[index + 1..])
                    .map(SetAlphaRenamingStrategy::with_input)
                    .map(Into::into)
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":b") || cmd.starts_with(":beta") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                parse_beta_reduction_strategy(&line[index + 1..])
                    .map(SetBetaReductionStrategy::with_input)
                    .map(Into::into)
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":p") || cmd.starts_with(":parse") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                Ok(ParseLambdaExpression::with_input(&line[index + 1..]).into())
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":") => Err(err_msg(format!("unknown command `{}`", cmd))),
        cmd => Ok(EvaluateLambdaExpression::with_input(cmd).into()),
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Quit;

impl Command for Quit {
    type Input = ();
    type Output = ColoredString;

    fn with_input(_input: <Self as Command>::Input) -> Self {
        Quit
    }

    fn execute(self, _ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        stop("Good bye!".green(), "")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Help;

impl Command for Help {
    type Input = ();
    type Output = ColoredString;

    fn with_input(_input: <Self as Command>::Input) -> Self {
        Help
    }

    fn execute(self, _ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        cont_output(help_message().green(), "")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Version;

impl Command for Version {
    type Input = ();
    type Output = ColoredString;

    fn with_input(_input: <Self as Command>::Input) -> Self {
        Version
    }

    fn execute(self, _ctx: &mut Context) -> Continuation<<Self as Command>::Output> {
        cont_output(version_message().blue(), "")
    }
}

fn welcome_message() -> String {
    format!(
        "Welcome to {}, the Lambda Calculus Repl, version {}",
        build::NAME,
        build::VERSION
    )
}

fn version_message() -> String {
    format!("{} version {}", build::NAME, build::VERSION)
}

fn help_message() -> String {
    r###"Usage:
    Type a lambda expression in classic notation and press enter to evaluate it.
    During evaluation beta-reduction is applied recursively to the given lambda
    expression. To avoid name clashes alpha-conversion is applied when
    appropriate.

Example:
    λ> (\x.(\y.x y) a) b [enter]
    => b a

Commands:
    :h or :help       displays this help information
    :q or :quit       quits the repl session
    :v or :version    prints out the version of lamcali
    :p <expr>         parses the lambda expression <expr> and prints out the
                      abstract syntax tree (AST) of the lambda expression
    :b or :beta       print current set β-reduction strategy
    :b app            set β-reduction strategy to applicative-order
                      reducing to normal form
    :b cbn            set β-reduction strategy to call-by-name
                      reducing to weak head normal form
    :b cbv            set β-reduction strategy to call-by-value
                      reducing to weak normal form
    :b hap            set β-reduction strategy to hybrid-applicative-order
                      reducing to normal form
    :b hno            set β-reduction strategy to hybrid-normal-order
                      reducing to normal form
    :b hsp            set β-reduction strategy to head-spine
                      reducing to head normal form
    :b nor            set β-reduction strategy to normal-order (the default)
                      reducing to normal form
    :a or :alpha      print current set α-conversion strategy
    :a enumerate      set α-conversion strategy to enumerate (the default)
                      (appending increasing digits)
    :a prime          set α-conversion strategy to prime
                      (appending tick symbols)

    the [arrow-up] and [arrow-down] keys lets you navigate through the
    history of typed commands and expressions and recall them.
    "###.to_string()
}
