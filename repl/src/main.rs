//! A command line repl (read-evaluate-print loop) for lambda calculus.

#![doc(html_root_url = "https://docs.rs/lamcal-repl/0.4.0/lamcali")]
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

#[macro_use]
extern crate failure;

mod build {
    pub const NAME: &str = env!("CARGO_PKG_NAME");
    pub const VERSION: &str = env!("CARGO_PKG_VERSION");
}

use std::{fmt::Display, fs, path::PathBuf};

use colored::*;
use failure::{err_msg, Error};
use rustyline::Editor;

use lamcal_repl::{
    command::*,
    context::Context,
    let_stmt::{parse_let_statement, LetStatement},
    model::{AlphaRenamingStrategy, BetaReductionStrategy},
    settings::Settings,
};

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
        Ok(history_file) => {
            if let Err(err) = rl.load_history(&history_file) {
                print_warning(err);
            }
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
        Ok(history_file) => {
            if let Err(err) = rl.save_history(&history_file) {
                print_error(err);
            }
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
    println!("{}\n", text)
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
    ToggleInspectedMode(ToggleInspectedMode),
    PrintAlphaRenamingStrategy(PrintAlphaRenamingStrategy),
    PrintBetaReductionStrategy(PrintBetaReductionStrategy),
    SetAlphaRenamingStrategy(SetAlphaRenamingStrategy),
    SetBetaReductionStrategy(SetBetaReductionStrategy),
    PrintLambdaExpression(ParseLambdaExpression<'a>),
    ExpandLambdaExpression(ExpandLambdaExpression<'a>),
    BetaReduceLambdaExpression(BetaReduceLambdaExpression<'a>),
    EvaluateLambdaExpression(EvaluateLambdaExpression<'a>),
    LetStatement(LetStatement),
    ClearEnvironment(ClearEnvironment),
    PrintEnvironment(PrintEnvironment),
    LoadBindings(LoadBindings),
}

impl<'a> LciCommand<'a> {
    fn execute(self, ctx: &mut Context) -> Continuation<()> {
        match self {
            LciCommand::Help(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::Quit(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::Version(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::ToggleInspectedMode(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintAlphaRenamingStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintBetaReductionStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::SetAlphaRenamingStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::SetBetaReductionStrategy(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintLambdaExpression(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::ExpandLambdaExpression(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::BetaReduceLambdaExpression(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::EvaluateLambdaExpression(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::LetStatement(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::ClearEnvironment(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::PrintEnvironment(cmd) => handle_continuation(cmd.execute(ctx)),
            LciCommand::LoadBindings(cmd) => handle_continuation(cmd.execute(ctx)),
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

impl<'a> From<ToggleInspectedMode> for LciCommand<'a> {
    fn from(cmd: ToggleInspectedMode) -> Self {
        LciCommand::ToggleInspectedMode(cmd)
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

impl<'a> From<ExpandLambdaExpression<'a>> for LciCommand<'a> {
    fn from(cmd: ExpandLambdaExpression<'a>) -> Self {
        LciCommand::ExpandLambdaExpression(cmd)
    }
}

impl<'a> From<BetaReduceLambdaExpression<'a>> for LciCommand<'a> {
    fn from(cmd: BetaReduceLambdaExpression<'a>) -> Self {
        LciCommand::BetaReduceLambdaExpression(cmd)
    }
}

impl<'a> From<EvaluateLambdaExpression<'a>> for LciCommand<'a> {
    fn from(cmd: EvaluateLambdaExpression<'a>) -> Self {
        LciCommand::EvaluateLambdaExpression(cmd)
    }
}

impl<'a> From<LetStatement> for LciCommand<'a> {
    fn from(cmd: LetStatement) -> Self {
        LciCommand::LetStatement(cmd)
    }
}

impl<'a> From<ClearEnvironment> for LciCommand<'a> {
    fn from(cmd: ClearEnvironment) -> Self {
        LciCommand::ClearEnvironment(cmd)
    }
}

impl<'a> From<PrintEnvironment> for LciCommand<'a> {
    fn from(cmd: PrintEnvironment) -> Self {
        LciCommand::PrintEnvironment(cmd)
    }
}

impl<'a> From<LoadBindings> for LciCommand<'a> {
    fn from(cmd: LoadBindings) -> Self {
        LciCommand::LoadBindings(cmd)
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
        "Welcome to {}, the Lambda Calculus REPL, version {}",
        build::NAME,
        build::VERSION
    )
}

fn version_message() -> String {
    format!("{} version {}", build::NAME, build::VERSION)
}

fn parse_command(line: &str) -> Result<LciCommand, Error> {
    match line.trim() {
        ":h" | ":help" => Ok(Help.into()),
        ":q" | ":quit" => Ok(Quit.into()),
        ":v" | ":version" => Ok(Version.into()),
        ":i" | ":inspected" => Ok(ToggleInspectedMode.into()),
        ":as" | ":alpha-strategy" => Ok(PrintAlphaRenamingStrategy.into()),
        ":bs" | ":beta-strategy" => Ok(PrintBetaReductionStrategy.into()),
        ":clr-env" => Ok(ClearEnvironment.into()),
        ":ls-env" => Ok(PrintEnvironment::with_input("".into()).into()),
        cmd if cmd.starts_with(":as ") || cmd.starts_with(":alpha-strategy ") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                parse_alpha_renaming_strategy(&cmd[index + 1..])
                    .map(SetAlphaRenamingStrategy::with_input)
                    .map(Into::into)
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":bs ") || cmd.starts_with(":beta-strategy ") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                parse_beta_reduction_strategy(&cmd[index + 1..])
                    .map(SetBetaReductionStrategy::with_input)
                    .map(Into::into)
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":e ") || cmd.starts_with(":eval ") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                Ok(EvaluateLambdaExpression::with_input(&cmd[index + 1..]).into())
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":b ") || cmd.starts_with(":beta ") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                Ok(BetaReduceLambdaExpression::with_input(&cmd[index + 1..]).into())
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":x ") || cmd.starts_with(":expand ") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                Ok(ExpandLambdaExpression::with_input(&cmd[index + 1..]).into())
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":p ") || cmd.starts_with(":parse ") => {
            if let Some(index) = cmd.find(char::is_whitespace) {
                Ok(ParseLambdaExpression::with_input(&cmd[index + 1..]).into())
            } else {
                Err(err_msg(format!("unknown command `{}`", cmd)))
            }
        },
        cmd if cmd.starts_with(":let ") => parse_let_statement(cmd).map(Into::into),
        cmd if cmd.starts_with(":ls-env ") => {
            Ok(PrintEnvironment::with_input(cmd[8..].trim().to_owned()).into())
        },
        cmd if cmd.starts_with(":ld-env ") => {
            Ok(LoadBindings::with_input(cmd[8..].trim().to_owned()).into())
        },
        cmd if cmd.starts_with(':') => Err(err_msg(format!("unknown command `{}`", cmd))),
        cmd => Ok(EvaluateLambdaExpression::with_input(cmd).into()),
    }
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

    :i or :inspected  toggle inspected mode on and off. In inspected mode the
                      result of each step during evaluation or reduction is
                      printed to the terminal.

    :e <expr> or :eval <expr>
                      evaluates the lambda expression <expr>. This command is
                      equivalent to just typing a lambda expression and
                      pressing [enter].

    :b <expr> or :beta <expr>
                      performs a beta-reduction on the lambda expression <expr>
                      using the current set strategy.

    :x <expr> or :expand <expr>
                      replaces free variables in the lambda expression <expr>
                      with the expression bound to the variable's name in the
                      current environment.

    :p <expr> or :parse <expr>
                      parses the lambda expression <expr> and prints out the
                      abstract syntax tree (AST) of the lambda expression.

    :bs or :beta-strategy
                      prints the current set beta-reduction strategy.

    :bs <strategy> or :beta_strategy <strategy>
                      set the beta-reduction strategy to <strategy>.
                      <strategy> can be one of:
                      app : applicative-order reducing to normal form
                      cbn : call-by-name reducing to weak head normal form
                      cbv : call-by-value reducing to weak normal form
                      hap : hybrid-applicative-order reducing to normal form
                      hno : hybrid-normal-order reducing to normal form
                      hsp : head-spine reducing to head normal form
                      nor : normal-order reducing to normal form (the default)

    :as or :alpha-strategy
                      prints the current set alpha-conversion strategy.

    :as <strategy> or :alpha-strategy <strategy>
                      set the alpha-conversion strategy to <strategy>.
                      <strategy> can be one of:
                      enumerate : appending increasing digits (the default)
                      prime     : appending tick symbols

    :let <name> = <expr>
                      defines a new binding of <expr> to <name> and adds it to
                      the environment. If a binding with the same name
                      previously existed in the environment it is replaced by
                      the new binding.

    :clr-env          clears the environment, all bindings are removed

    :ld-env default   loads the default set of predefined bindings into the
                      environment. Existing bindings with the same name as a
                      binding in the default set will be replaced. Existing
                      bindings with no name clash will not be changed.

    :ls-env           lists all bindings defined in the environment

    :ls-env <pattern> lists all bindings filtered by <pattern>. It lists all
                      bindings with a name that contains the given pattern as a
                      substring (ignoring case).

    the [arrow-up] and [arrow-down] keys lets you navigate through the
    history of typed commands and expressions and recall them.
    "###
    .to_string()
}
