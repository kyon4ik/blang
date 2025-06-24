use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use blang::diagnostics::SourceMap;
use blang::lexer::{Lexer, TokenKind};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    input: PathBuf,
}

fn main() {
    let args = Args::parse();

    let mut input_file = File::open(&args.input).unwrap();
    let mut src = Vec::new();
    input_file.read_to_end(&mut src).unwrap();

    let mut lexer = Lexer::new(&src);
    let src_map = SourceMap::new(&src, &args.input);

    let mut prev_line = 0;
    loop {
        let token = lexer.next_token();

        if token.kind == TokenKind::Eof {
            break;
        }

        let (start_loc, end_loc) = src_map.locate_span(token.span).unwrap();
        assert_eq!(start_loc.line, end_loc.line);
        if prev_line != start_loc.line {
            println!("{:>3} | {}", start_loc.line, token.kind);
            prev_line = start_loc.line;
        } else {
            println!("     | {}", token.kind);
        }
    }
}
