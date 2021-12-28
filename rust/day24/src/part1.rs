use std::io;
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Term {
    Int(i64),
    Unknown(usize),
    Expr(Op, Box<Term>, Box<Term>)
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

type State = [Term; 4];

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

fn i2c(i: usize) -> u8 {
    assert!(0 <= i && i < 4);
    i as u8 + b'w'
}

fn operate(op: &Op, x: i64, y: i64) -> i64 {
    match op {
        Op::Add => x + y,
        Op::Multiply => x * y,
        Op::Divide => x / y,
        Op::Modulo => x % y,
        Op::Equal => if x == y { 1 } else { 0 },
    }
}

fn reduce(t: &Term) -> Term {
    match t {
        Term::Expr(op, left, right) => 
            match op {
                Op::Add => {
                    if let Term::Int(0) = **left {
                        reduce(right)
                    } else if let Term::Int(0) = **right {
                        reduce(left)
                    } else {
                        t.clone()
                    }
                },
                Op::Multiply => {
                    if let Term::Int(0) = **left {
                        Term::Int(0)
                    } else if let Term::Int(0) = **right {
                        Term::Int(0)
                    } else if let Term::Int(1) = **left {
                        reduce(right)
                    } else if let Term::Int(1) = **right {
                        reduce(left)
                    } else {
                        t.clone()
                    }
                },
                Op::Divide => {
                    if let Term::Int(0) = **left {
                        Term::Int(0)
                    } else if let Term::Int(1) = **right {
                        reduce(left)
                    } else {
                        t.clone()
                    }
                },
                Op::Modulo => {
                    if let Term::Int(0) = **left {
                        Term::Int(0)
                    } else if let Term::Int(1) = **right {
                        Term::Int(0)
                    } else {
                        t.clone()
                    }
                }
                Op::Equal => {
                    if **left == **right {
                        Term::Int(1)
                    } else {
                        t.clone()
                    }
                }
            }
        _ => t.clone(),
    }
}

fn perform(cmd: &Command, s: &mut State, uctr: &mut usize) {
    match cmd {
        Command::Input(c) => {
            let id = *uctr;
            *uctr += 1;
            s[c2i(*c)] = Term::Unknown(id);
        },
        // TODO: Bad code, too much cloning
        Command::Operation(op, var, operand) => {
            let var_i = c2i(*var);
            let var_term = &s[var_i];
            let op_term = match operand {
                Operand::Const(x) => Term::Int(*x),
                Operand::Var(v) => s[c2i(*v)].clone()
            };
            if let (Term::Int(v0), Term::Int(v1)) = (var_term, op_term.clone()) {
                s[var_i] = Term::Int(operate(op, *v0, v1));
            } else {
                s[var_i] = reduce(&Term::Expr(*op, Box::new(var_term.clone()), Box::new(op_term)));
            }
        }
    }
}

fn evaluate(term: &Term, values: &[i64]) -> i64 {
    match term {
        Term::Int(x) => *x,
        Term::Unknown(id) => values[*id],
        Term::Expr(op, left, right) => {
            let lv = evaluate(left, values);
            let rv = evaluate(right, values);
            match op {
                Op::Add => lv + rv,
                Op::Multiply => lv * rv,
                Op::Divide => lv / rv,
                Op::Modulo => lv % rv,
                Op::Equal => if lv == rv { 1 } else { 0 },
            }
        }
    }
}

fn main() {
    let mut state: State = [Term::Int(0), Term::Int(0), Term::Int(0), Term::Int(0)];
    let mut unknown_counter = 0;
    let mut i = 0;
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
        if bytes == 0 {
            break;
        }

        let cmd = parse_cmd(&line);

        perform(&cmd, &mut state, &mut unknown_counter);

        i += 1;
        eprintln!("{}", i);
        eprintln!("{:?}", state);
    }
    // println!("{:?}", state);
//     println!("State computed.");
//     let values = [9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9];
//     for i in 0..1000000 {
//         println!("{}-result: {}", i, evaluate(&state[c2i(b'z')], &values));
//     }
}
