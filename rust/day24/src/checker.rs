use std::io;
use std::env;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    Multiply,
    Divide,
    Modulo,
    Equal,
}

#[derive(Debug)]
enum Operand {
    Var(u8),
    Const(i64)
}

#[derive(Debug)]
enum Command {
    Input(u8),
    Operation(Op, u8, Operand)
}

fn first_u8(s: &str) -> u8 {
    *s.as_bytes().first().unwrap()
}

fn str2operand(s: &str) -> Operand {
    let fc = first_u8(s);
    if b'w' <= fc && fc <= b'z' {
        Operand::Var(fc)
    } else {
        Operand::Const(s.parse().unwrap())
    }
}

fn parse_cmd(s: &str) -> Command {
    let v: Vec<&str> = s.trim().split(' ').collect();
    
    if v[0] == "inp" {
        assert!(v.len() == 2);
        Command::Input(first_u8(v[1]))
    } else {
        assert!(v.len() == 3);
        let op = match v[0] {
            "add" => Op::Add,
            "mul" => Op::Multiply,
            "div" => Op::Divide,
            "mod" => Op::Modulo,
            "eql" => Op::Equal,
            _ => panic!("Not a proper command!")
        };
        Command::Operation(op, first_u8(v[1]), str2operand(v[2]))
    }
}

fn c2i(c: u8) -> usize {
    assert!(b'w' <= c && c <= b'z');
    (c - b'w') as usize
}

fn get_input_values() -> Vec<i64> {
    let v: Vec<String> = env::args().collect();
    assert!(v.len() == 2);
    v[1].as_bytes().iter().map(|c| (c - b'0') as i64).collect()
}

fn main() {
    let mut state = [0, 0, 0, 0];
    let input_values = get_input_values();
    let mut input_i = 0;
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
        
        if bytes == 0 {
            break;
        }

        match parse_cmd(&line) {
            Command::Input(vi) => {
                state[c2i(vi)] = input_values[input_i];
                input_i += 1;
            },
            Command::Operation(op, target, operand) => {
                let tr = c2i(target);
                let left = state[tr];
                let right = match operand {
                    Operand::Var(source) => state[c2i(source)],
                    Operand::Const(x) => x
                };
                let result = match op {
                    Op::Add => left + right,
                    Op::Multiply => left * right,
                    Op::Divide => left / right,
                    Op::Modulo => left % right,
                    Op::Equal => if left == right { 1 } else { 0 },
                };
                state[tr] = result;
            }
        }
    }

    println!("{}", if state[c2i(b'z')] == 0 { "Success!" } else { "Failure." });
}
