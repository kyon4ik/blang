use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use blang::lexer::{Lexer, TokenKind};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    input: PathBuf,
}

fn main() {
    let args = Args::parse();

    let mut input_file = File::open(args.input).unwrap();
    let mut src = Vec::new();
    input_file.read_to_end(&mut src).unwrap();

    let mut lexer = Lexer::new(&src);
    loop {
        let token = lexer.next_token();
        println!("{}..{}: {}", token.span.start, token.span.end, token.kind);

        if token.kind == TokenKind::Eof {
            break;
        }
    }
}
