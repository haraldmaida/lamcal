extern crate colored;
extern crate config;
extern crate failure;
extern crate rustyline;

mod settings;

use std::env;
use std::fs;
use std::path::PathBuf;

use failure::{Context, Error};
use rustyline::Editor;

use settings::Settings;

fn main() {
    let settings = if let Ok(settings_file) = settings_file() {
        match Settings::default().merge(&settings_file) {
            Ok(settings) => settings,
            Err(err) => {
                println!("error: {}", err);
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
            println!("warning: {}", err);
        },
        Err(err) => println!("warning: {}", err),
    }
    const PROMPT_HEAD: &str = "λ> ";
    let mut prompt = PROMPT_HEAD.to_owned();
    loop {
        match rl.readline(&prompt) {
            Ok(line) => {
                rl.add_history_entry(&line);
                let continuation = if line.is_empty() {
                    next(format!(""))
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
                println!("{}", err);
                break;
            },
        }
    }
    match history_file() {
        Ok(history_file) => if let Err(err) = rl.save_history(&history_file) {
            println!("error: {}", err);
        },
        Err(err) => println!("error: {}", err),
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

fn next(prompt: String) -> Continuation {
    Continuation {
        processing: Processing::Continue,
        prompt,
    }
}

fn stop(prompt: String) -> Continuation {
    Continuation {
        processing: Processing::Stop,
        prompt,
    }
}

fn evaluate_expression(line: &str) -> Continuation {
    unimplemented!()
}

fn process_command(line: &str) -> Continuation {
    match line.trim() {
        ":h" | ":help" => next(help_message()),
        ":q" | ":quit" => stop(format!("Good bye!")),
        _ => next(format!("")),
    }
}

fn help_message() -> String {
    unimplemented!()
}
