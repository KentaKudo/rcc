use std::env;

#[derive(Debug, PartialEq)]
enum TokenKind {
    Reserved(char),
    Num(i64),
    EOF,
}

#[derive(Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    next: Option<Box<Token>>,
}

impl Token {
    fn consume(&self, op: char) -> Option<&Token> {
        if self.kind != TokenKind::Reserved(op) {
            return None;
        }

        self.next.as_ref().map(|t| t.as_ref())
    }

    fn consume_number(&self) -> Option<(i64, &Token)> {
        if let TokenKind::Num(nr) = self.kind {
            if let Some(next) = self.next.as_ref() {
                return Some((nr, next.as_ref()));
            }
        }

        None
    }

    fn is_eof(&self) -> bool {
        return self.kind == TokenKind::EOF;
    }

    fn append(&mut self, kind: TokenKind) -> &mut Token {
        let t = Token { kind, next: None };
        self.next = Some(Box::new(t));
        self.next.as_mut().unwrap()
    }
}

fn tokenise(p: &str) -> Result<Token, String> {
    let mut head = Token {
        kind: TokenKind::EOF,
        next: None,
    };
    let mut cur = &mut head;

    let mut input = p;
    while input != "" {
        let c = input.chars().nth(0).unwrap();
        if c.is_ascii_whitespace() {
            input = &input[1..];
            continue;
        }

        if c == '+' || c == '-' {
            input = &input[1..];
            cur = cur.append(TokenKind::Reserved(c));
            continue;
        }

        if c.is_ascii_digit() {
            let (d, rest) = strtol(input);
            cur = cur.append(TokenKind::Num(
                d.ok_or(format!("予期しない文字です: {}", input))?,
            ));
            input = rest;
            continue;
        }

        return Err("トークナイズできません".to_string());
    }

    cur.append(TokenKind::EOF);

    Ok(match head.next {
        Some(t) => *t,
        None => head,
    })
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err("引数の個数が正しくありません".to_string());
    }

    let token = tokenise(&args[1])?;

    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");

    let (d, mut token) = token
        .consume_number()
        .ok_or("数ではありません".to_string())?;
    println!("  mov rax, {}", d);

    while !token.is_eof() {
        let next = match token.consume('+') {
            Some(next) => {
                let (d, next) = next
                    .consume_number()
                    .ok_or("数ではありません".to_string())?;
                println!("  add rax, {}", d);
                next
            }
            None => match token.consume('-') {
                Some(next) => {
                    let (d, next) = next
                        .consume_number()
                        .ok_or("数ではありません".to_string())?;
                    println!("  sub rax, {}", d);
                    next
                }
                None => return Err(format!("'{}'ではありません", '-')),
            },
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
    fn successfully_tokenise() -> Result<(), String> {
        assert_eq!(
            Token {
                kind: TokenKind::Num(12),
                next: Some(Box::new(Token {
                    kind: TokenKind::Reserved('+'),
                    next: Some(Box::new(Token {
                        kind: TokenKind::Num(34),
                        next: Some(Box::new(Token {
                            kind: TokenKind::Reserved('-'),
                            next: Some(Box::new(Token {
                                kind: TokenKind::Num(5),
                                next: Some(Box::new(Token {
                                    kind: TokenKind::EOF,
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
    fn tokenise_empty() -> Result<(), String> {
        assert_eq!(
            Token {
                kind: TokenKind::EOF,
                next: None,
            },
            tokenise("  ")?
        );
        Ok(())
    }
}
