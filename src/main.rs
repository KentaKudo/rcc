use std::env;
use std::process;
use std::str;

extern crate rcc;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません");
        process::exit(1);
    }

    if let Err(e) = run(&args[1]) {
        eprintln!("{}", &args[1]);
        eprintln!("{}", e);
        process::exit(1);
    }
}

fn run(input: &str) -> rcc::Result<()> {
    let token = rcc::parse::tokenise(input)?;
    let (node, _) = rcc::parse::expr(&token)?;

    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");

    rcc::codegen::gen(&node);

    println!("  pop rax");
    println!("  ret");
    Ok(())
}
