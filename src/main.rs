use std::env;
use std::fmt;
use std::process;
use std::str;

#[derive(Debug, PartialEq, Default, Copy, Clone)]
struct Location(u64);

// TODO: rename
#[derive(Debug)]
struct CustomError(String, Location);

impl fmt::Display for CustomError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CustomError(msg, loc) = self;
        writeln!(f, "{}^ {}", " ".repeat(loc.0 as usize), msg)
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    Reserved(char),
    Num(i64),
    EOF,
}

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::EOF
    }
}

/// Token linked list
#[derive(Debug, PartialEq, Default)]
struct Token {
    kind: TokenKind,
    loc: Location,
    next: Option<Box<Token>>,
}

impl Token {
    fn consume_reserved(&self) -> Option<(char, &Token)> {
        if let TokenKind::Reserved(c) = self.kind {
            return Some((c, self.next.as_ref().unwrap().as_ref()));
        }

        None
    }

    fn expect_reserved(&self) -> Result<(char, &Token), CustomError> {
        self.consume_reserved().ok_or(CustomError(
            "有効な文字ではありません".to_string(),
            self.loc,
        ))
    }

    fn consume_number(&self) -> Option<(i64, &Token)> {
        if let TokenKind::Num(nr) = self.kind {
            return Some((nr, self.next.as_ref().unwrap().as_ref()));
        }

        None
    }

    fn expect_number(&self) -> Result<(i64, &Token), CustomError> {
        self.consume_number()
            .ok_or(CustomError("数字ではありません".to_string(), self.loc))
    }

    fn is_eof(&self) -> bool {
        return self.kind == TokenKind::EOF;
    }

    fn append(&mut self, kind: TokenKind, loc: Location) -> &mut Token {
        let t = Token {
            kind,
            loc,
            next: None,
        };
        self.next = Some(Box::new(t));
        self.next.as_mut().unwrap()
    }
}

fn tokenise(p: &str) -> Result<Token, CustomError> {
    let mut head = Token::default();
    let mut cur = &mut head;

    let mut input = p;
    while input != "" {
        let c = input.chars().nth(0).unwrap();
        let rest = match c {
            _ if c.is_ascii_whitespace() => &input[1..],
            _ if c == '+' || c == '-' => {
                cur = cur.append(
                    TokenKind::Reserved(c),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            _ if c.is_ascii_digit() => {
                let (d, rest) = strtol(input);
                cur = cur.append(
                    TokenKind::Num(d.ok_or(CustomError(
                        format!("予期しない文字です: {}", input),
                        cur.loc,
                    ))?),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                rest
            }
            _ => {
                return Err(CustomError(
                    "トークナイズできません".to_string(),
                    Location((p.chars().count() - input.chars().count()) as u64),
                ))
            }
        };

        input = rest;
    }

    cur.append(TokenKind::EOF, Location(p.chars().count() as u64));

    Ok(head.next.map(|b| *b).unwrap_or_default())
}

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

fn run(input: &str) -> Result<(), CustomError> {
    let token = tokenise(input)?;

    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");

    let (d, mut token) = token.expect_number()?;
    println!("  mov rax, {}", d);

    while !token.is_eof() {
        let (res, next) = token.expect_reserved()?;
        let next = match res {
            '+' => {
                let (d, next) = next.expect_number()?;
                println!("  add rax, {}", d);
                next
            }
            '-' => {
                let (d, next) = next.expect_number()?;
                println!("  sub rax, {}", d);
                next
            }
            _ => {
                return Err(CustomError(
                    format!("予期しない文字です: {}", res),
                    token.loc,
                ))
            }
        };

        token = next;
    }

    println!("  ret");
    Ok(())
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

    #[test]
    fn successfully_tokenise() -> Result<(), CustomError> {
        assert_eq!(
            Token {
                kind: TokenKind::Num(12),
                loc: Location(1),
                next: Some(Box::new(Token {
                    kind: TokenKind::Reserved('+'),
                    loc: Location(4),
                    next: Some(Box::new(Token {
                        kind: TokenKind::Num(34),
                        loc: Location(6),
                        next: Some(Box::new(Token {
                            kind: TokenKind::Reserved('-'),
                            loc: Location(9),
                            next: Some(Box::new(Token {
                                kind: TokenKind::Num(5),
                                loc: Location(11),
                                next: Some(Box::new(Token {
                                    kind: TokenKind::EOF,
                                    loc: Location(13),
                                    next: None,
                                }))
                            }))
                        }))
                    }))
                }))
            },
            tokenise(" 12 + 34 - 5 ")?
        );
        Ok(())
    }

    #[test]
    fn tokenise_empty() -> Result<(), CustomError> {
        assert_eq!(
            Token {
                kind: TokenKind::EOF,
                loc: Location(2),
                next: None,
            },
            tokenise("  ")?
        );
        Ok(())
    }
}
