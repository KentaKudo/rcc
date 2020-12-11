use crate::parse::{Node, NodeKind};

pub fn gen(node: &Node) {
    if let NodeKind::Num(n) = node.kind {
        println!("  push {}", n);
        return;
    }

    gen(&node.lhs.as_ref().unwrap());
    gen(&node.rhs.as_ref().unwrap());

    println!("  pop rdi");
    println!("  pop rax");

    match node.kind {
        NodeKind::Add => println!("  add rax, rdi"),
        NodeKind::Sub => println!("  sub rax, rdi"),
        NodeKind::Mul => println!("  imul rax, rdi"),
        NodeKind::Div => {
            println!("  cqo");
            println!("  idiv rdi");
        }
        NodeKind::Equal => {
            println!("  cmp rax, rdi");
            println!("  sete al");
            println!("  movzb rax, al");
        }
        NodeKind::NotEqual => {
            println!("  cmp rax, rdi");
            println!("  setne al");
            println!("  movzb rax, al");
        }
        NodeKind::Less => {
            println!("  cmp rax, rdi");
            println!("  setl al");
            println!("  movzb rax, al");
        }
        NodeKind::LessOrEqual => {
            println!("  cmp rax, rdi");
            println!("  setle al");
            println!("  movzb rax, al");
        }
        _ => {}
    }

    println!("  push rax");
}
