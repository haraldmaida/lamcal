extern crate colored;
extern crate config;
extern crate failure;
extern crate rustyline;

extern crate lamcal;

mod settings;

mod build {
    pub const NAME: &str = env!("CARGO_PKG_NAME");
    pub const VERSION: &str = env!("CARGO_PKG_VERSION");
}

use std::env;
use std::fmt::Display;
use std::fs;
use std::path::PathBuf;

use colored::*;
use failure::{Context, Error};
use rustyline::Editor;

use lamcal::parse;

use settings::Settings;

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
    let mut rl = Editor::<()>::new()
        .history_ignore_dups(settings.history.ignore_dups)
        .history_ignore_space(settings.history.ignore_space);
    rl.set_history_max_len(settings.history.max_len);
    match history_file() {
        Ok(history_file) => if let Err(err) = rl.load_history(&history_file) {
            print_warning(err);
        },
        Err(err) => print_warning(err),
    }
    let mut prompt = PROMPT_HEAD.to_owned();
    loop {
        match rl.readline(&prompt) {
            Ok(line) => {
                rl.add_history_entry(&line);
                let continuation = if line.is_empty() {
                    next(PROMPT_HEAD.to_owned())
                } else if line.starts_with(':') {
                    process_command(&line)
                } else {
                    evaluate_expression(&line)
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
    println!("{}", prompt);
}

fn app_dir() -> Result<PathBuf, Error> {
    let mut path = env::home_dir().ok_or_else(|| {
        Error::from(Context::from(
            "The OS environment has no home directory defined",
        ))
    })?;
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

struct Continuation {
    processing: Processing,
    prompt: String,
}

enum Processing {
    Continue,
    Stop,
}

fn next(prompt: impl Display) -> Continuation {
    Continuation {
        processing: Processing::Continue,
        prompt: prompt.to_string(),
    }
}

fn stop(prompt: impl Display) -> Continuation {
    Continuation {
        processing: Processing::Stop,
        prompt: prompt.to_string(),
    }
}

fn print(text: impl Display) {
    println!("{}", text)
}

fn print_error(error: impl Display) {
    println!("{} {}", format!("error:").red(), error)
}

fn print_warning(warning: impl Display) {
    println!("{} {}", format!("warning:").yellow(), warning)
}

fn print_info(info: impl Display) {
    println!("{} {}", format!("info:").blue(), info)
}

fn evaluate_expression(line: &str) -> Continuation {
    match parse(line.chars()) {
        Ok(mut expr) => {
            expr.reduce();
            print(expr);
        },
        Err(err) => {
            print_error(err);
        },
    }
    next("")
}

fn process_command(line: &str) -> Continuation {
    match line.trim() {
        ":h" | ":help" => {
            print(help_message().green());
            next("")
        },
        ":q" | ":quit" => {
            print(format!("Good bye!").green());
            stop("")
        },
        ":v" | ":version" => {
            print(version_message().blue());
            next("")
        },
        cmd => {
            print_error(format!("unknown command `{}`", cmd));
            next("")
        },
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
    unimplemented!()
}
