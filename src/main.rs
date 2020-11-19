use std::env;

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

#[derive(Debug, PartialEq, Default)]
struct Token {
    kind: TokenKind,
    next: Option<Box<Token>>,
}

impl Token {
    fn consume_reserved(&self) -> Result<(char, &Token), String> {
        if let TokenKind::Reserved(c) = self.kind {
            return Ok((c, self.next.as_ref().unwrap().as_ref()));
        }

        Err("有効な文字ではありません".to_string())
    }

    fn consume_number(&self) -> Result<(i64, &Token), String> {
        if let TokenKind::Num(nr) = self.kind {
            return Ok((nr, self.next.as_ref().unwrap().as_ref()));
        }

        Err("数字ではありません".to_string())
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
    let mut head = Token::default();
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

    Ok(head.next.map(|b| *b).unwrap_or_default())
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

    let (d, mut token) = token.consume_number()?;
    println!("  mov rax, {}", d);

    while !token.is_eof() {
        let (res, next) = token.consume_reserved()?;
        let next = match res {
            '+' => {
                let (d, next) = next.consume_number()?;
                println!("  add rax, {}", d);
                next
            }
            '-' => {
                let (d, next) = next.consume_number()?;
                println!("  sub rax, {}", d);
                next
            }
            _ => return Err(format!("予期しない文字です: {}", res)),
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
