use crate::utils;
use crate::CustomError;
use crate::Location;
use crate::Result;

#[derive(Debug, PartialEq)]
enum TokenKind {
    Reserved(&'static str),
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
pub struct Token {
    kind: TokenKind,
    loc: Location,
    next: Option<Box<Token>>,
}

impl Token {
    fn consume_reserved(&self) -> Option<(&str, &Self)> {
        if let TokenKind::Reserved(s) = self.kind {
            return Some((s, self.next.as_ref().unwrap().as_ref()));
        }

        None
    }

    fn expect_reserved(&self) -> Result<(&str, &Self)> {
        self.consume_reserved().ok_or(CustomError(
            "有効な文字ではありません".to_string(),
            self.loc,
        ))
    }

    fn consume_number(&self) -> Option<(i64, &Self)> {
        if let TokenKind::Num(nr) = self.kind {
            return Some((nr, self.next.as_ref().unwrap().as_ref()));
        }

        None
    }

    fn expect_number(&self) -> Result<(i64, &Self)> {
        self.consume_number()
            .ok_or(CustomError("数字ではありません".to_string(), self.loc))
    }

    fn append(&mut self, kind: TokenKind, loc: Location) -> &mut Self {
        let t = Token {
            kind,
            loc,
            next: None,
        };
        self.next = Some(Box::new(t));
        self.next.as_mut().unwrap()
    }
}

pub fn tokenise(p: &str) -> Result<Token> {
    let mut head = Token::default();
    let mut cur = &mut head;

    let mut input = p;
    while input != "" {
        let c = input.chars().nth(0).unwrap();
        let rest = match c {
            _ if c.is_ascii_whitespace() => &input[1..],
            '=' if input.chars().nth(1).filter(|c| *c == '=').is_some() => {
                cur = cur.append(
                    TokenKind::Reserved("=="),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[2..]
            }
            '!' if input.chars().nth(1).filter(|c| *c == '=').is_some() => {
                cur = cur.append(
                    TokenKind::Reserved("!="),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[2..]
            }
            '>' if input.chars().nth(1).filter(|c| *c == '=').is_some() => {
                cur = cur.append(
                    TokenKind::Reserved(">="),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[2..]
            }
            '<' if input.chars().nth(1).filter(|c| *c == '=').is_some() => {
                cur = cur.append(
                    TokenKind::Reserved("<="),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[2..]
            }
            '+' => {
                cur = cur.append(
                    TokenKind::Reserved("+"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            '-' => {
                cur = cur.append(
                    TokenKind::Reserved("-"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            '*' => {
                cur = cur.append(
                    TokenKind::Reserved("*"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            '/' => {
                cur = cur.append(
                    TokenKind::Reserved("/"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            '(' => {
                cur = cur.append(
                    TokenKind::Reserved("("),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            ')' => {
                cur = cur.append(
                    TokenKind::Reserved(")"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            '<' => {
                cur = cur.append(
                    TokenKind::Reserved("<"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            '>' => {
                cur = cur.append(
                    TokenKind::Reserved(">"),
                    Location((p.chars().count() - input.chars().count()) as u64),
                );
                &input[1..]
            }
            _ if c.is_ascii_digit() => {
                let (d, rest) = utils::strtol(input);
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

pub enum NodeKind {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Num(i64),
}

pub struct Node {
    pub kind: NodeKind,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
}

impl Node {
    fn new(kind: NodeKind, lhs: Option<Self>, rhs: Option<Self>) -> Self {
        Node {
            kind,
            lhs: lhs.map(|n| Box::new(n)),
            rhs: rhs.map(|n| Box::new(n)),
        }
    }

    fn new_number(n: i64) -> Self {
        Node {
            kind: NodeKind::Num(n),
            lhs: None,
            rhs: None,
        }
    }
}

pub fn expr(token: &Token) -> Result<(Node, &Token)> {
    equality(token)
}

fn equality(token: &Token) -> Result<(Node, &Token)> {
    let (mut root, mut token) = relational(token)?;

    loop {
        let (node, next) = match token.consume_reserved() {
            Some(("==", next)) => {
                let (rhs, next) = relational(next)?;
                (Node::new(NodeKind::Equal, Some(root), Some(rhs)), next)
            }
            Some(("!=", next)) => {
                let (rhs, next) = relational(next)?;
                (Node::new(NodeKind::NotEqual, Some(root), Some(rhs)), next)
            }
            _ => break,
        };

        root = node;
        token = next;
    }

    Ok((root, token))
}

fn relational(token: &Token) -> Result<(Node, &Token)> {
    let (mut root, mut token) = add(token)?;

    loop {
        let (node, next) = match token.consume_reserved() {
            Some((">", next)) => {
                let (lhs, next) = add(next)?;
                (Node::new(NodeKind::Less, Some(lhs), Some(root)), next)
            }
            Some((">=", next)) => {
                let (lhs, next) = add(next)?;
                (
                    Node::new(NodeKind::LessOrEqual, Some(lhs), Some(root)),
                    next,
                )
            }
            Some(("<", next)) => {
                let (rhs, next) = add(next)?;
                (Node::new(NodeKind::Less, Some(root), Some(rhs)), next)
            }
            Some(("<=", next)) => {
                let (rhs, next) = add(next)?;
                (
                    Node::new(NodeKind::LessOrEqual, Some(root), Some(rhs)),
                    next,
                )
            }
            _ => break,
        };

        root = node;
        token = next;
    }

    Ok((root, token))
}

fn add(token: &Token) -> Result<(Node, &Token)> {
    let (mut root, mut token) = mul(token)?;

    loop {
        let (node, next) = match token.consume_reserved() {
            Some(("+", next)) => {
                let (rhs, next) = mul(next)?;
                (Node::new(NodeKind::Add, Some(root), Some(rhs)), next)
            }
            Some(("-", next)) => {
                let (rhs, next) = mul(next)?;
                (Node::new(NodeKind::Sub, Some(root), Some(rhs)), next)
            }
            _ => break,
        };

        root = node;
        token = next;
    }

    Ok((root, token))
}

fn mul(token: &Token) -> Result<(Node, &Token)> {
    let (mut root, mut token) = unary(token)?;

    loop {
        let (node, next) = match token.consume_reserved() {
            Some(("*", next)) => {
                let (rhs, next) = unary(next)?;
                (Node::new(NodeKind::Mul, Some(root), Some(rhs)), next)
            }
            Some(("/", next)) => {
                let (rhs, next) = unary(next)?;
                (Node::new(NodeKind::Div, Some(root), Some(rhs)), next)
            }
            _ => break,
        };

        root = node;
        token = next;
    }

    Ok((root, token))
}

fn unary(token: &Token) -> Result<(Node, &Token)> {
    match token.consume_reserved() {
        Some(("+", next)) => primary(next),
        Some(("-", next)) => {
            let (rhs, next) = primary(next)?;
            let node = Node::new(NodeKind::Sub, Some(Node::new_number(0)), Some(rhs));
            Ok((node, next))
        }
        _ => primary(token),
    }
}

fn primary(token: &Token) -> Result<(Node, &Token)> {
    if let Some(("(", next)) = token.consume_reserved() {
        let (node, next) = expr(next)?;
        let (c, next) = next.expect_reserved()?;
        if c != ")" {
            return Err(CustomError(format!("予期しない文字です: {}", c), next.loc));
        }

        return Ok((node, next));
    }

    token
        .expect_number()
        .map(|(n, next)| (Node::new_number(n), next))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn successfully_tokenise() -> Result<()> {
        assert_eq!(
            Token {
                kind: TokenKind::Num(12),
                loc: Location(1),
                next: Some(Box::new(Token {
                    kind: TokenKind::Reserved("+"),
                    loc: Location(4),
                    next: Some(Box::new(Token {
                        kind: TokenKind::Num(34),
                        loc: Location(6),
                        next: Some(Box::new(Token {
                            kind: TokenKind::Reserved("-"),
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

        assert_eq!(
            Token {
                kind: TokenKind::Num(20),
                loc: Location(0),
                next: Some(Box::new(Token {
                    kind: TokenKind::Reserved("-"),
                    loc: Location(2),
                    next: Some(Box::new(Token {
                        kind: TokenKind::Num(3),
                        loc: Location(3),
                        next: Some(Box::new(Token {
                            kind: TokenKind::Reserved("*"),
                            loc: Location(4),
                            next: Some(Box::new(Token {
                                kind: TokenKind::Reserved("+"),
                                loc: Location(5),
                                next: Some(Box::new(Token {
                                    kind: TokenKind::Num(5),
                                    loc: Location(6),
                                    next: Some(Box::new(Token {
                                        kind: TokenKind::Reserved("<="),
                                        loc: Location(8),
                                        next: Some(Box::new(Token {
                                            kind: TokenKind::Num(5),
                                            loc: Location(11),
                                            next: Some(Box::new(Token {
                                                kind: TokenKind::EOF,
                                                loc: Location(12),
                                                next: None,
                                            }))
                                        }))
                                    })),
                                }))
                            }))
                        }))
                    }))
                }))
            },
            tokenise("20-3*+5 <= 5")?
        );

        Ok(())
    }

    #[test]
    fn tokenise_empty() -> Result<()> {
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
