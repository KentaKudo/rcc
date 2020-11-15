use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません");
        process::exit(1);
    }

    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");

    let input = &args[1];
    let (d, mut input) = match strtol(&input) {
        (Some(d), input) => (d, input),
        (None, input) => {
            eprintln!("予期しない文字です: '{}'", input);
            process::exit(1);
        }
    };
    println!("  mov rax, {}", d);

    while input != "" {
        if input.chars().nth(0).unwrap() == '+' {
            input = &input[1..];
            let (d, rest) = match strtol(input) {
                (Some(d), rest) => (d, rest),
                (None, rest) => {
                    eprintln!("予期しない文字です: '{}'", rest);
                    process::exit(1);
                }
            };
            println!("  add rax, {}", d);
            input = rest;
            continue;
        }

        if input.chars().nth(0).unwrap() == '-' {
            input = &input[1..];
            let (d, rest) = match strtol(input) {
                (Some(d), rest) => (d, rest),
                (None, rest) => {
                    eprintln!("予期しない文字です: '{}'", rest);
                    process::exit(1);
                }
            };
            println!("  sub rax, {}", d);
            input = rest;
            continue;
        }

        eprintln!("予期しない文字です: '{}'", input);
        process::exit(1);
    }

    println!("  ret");
    process::exit(0);
}

fn strtol(s: &str) -> (Option<i64>, &str) {
    let mut nr = "".to_string();
    for (idx, c) in s.char_indices() {
        if !c.is_ascii_digit() {
            let d = nr.parse::<i64>().ok();
            let rest = &s[idx..];
            return (d, rest);
        }

        nr.push(c);
    }

    let d = nr.parse::<i64>().ok();
    (d, "")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strtol_parse() {
        assert_eq!((Some(5), "+20-4"), strtol("5+20-4"));
        assert_eq!((None, "+20-4"), strtol("+20-4"));
        assert_eq!((Some(20), "-4"), strtol("20-4"));
        assert_eq!((None, "-4"), strtol("-4"));
        assert_eq!((Some(4), ""), strtol("4"));
        assert_eq!((None, ""), strtol(""));
    }
}
