extern crate colored;
extern crate failure;
extern crate rustyline;

use std::env;
use std::fs;
use std::path::PathBuf;

use failure::{Context, Error};
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();
    match history_file() {
        Ok(history_file) => if let Err(err) = rl.load_history(&history_file) {
            println!("warning: {}", err);
        },
        Err(err) => println!("warning: {}", err),
    }
    loop {
        const PROMPT_HEAD: &str = "λ> ";
        let mut prompt = PROMPT_HEAD;
        match rl.readline(prompt) {
            Ok(line) => {
                rl.add_history_entry(&line);
                if line.starts_with(':') {
                    process_command(&line);
                } else {
                    evaluate_expression(&line);
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

fn history_file() -> Result<PathBuf, Error> {
    app_dir().map(|mut path| {
        path.push("history.txt");
        path
    })
}

fn process_command(line: &str) {}

fn evaluate_expression(line: &str) {}
