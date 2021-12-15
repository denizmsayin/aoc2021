use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn get_closing_par(c: char) -> char
{
    match c {
        '(' => ')',
        '{' => '}',
        '[' => ']',
        '<' => '>',
        _ => panic!("Not a parenthesis")
    }
}

fn is_opening_par(c: char) -> bool {
    return c == '(' || c == '{' || c == '[' || c == '<';
}

fn corruption_score(s: &str) -> u64
{
    let mut stack: Vec<char> = Vec::new();

    for c in s.chars() {
        if is_opening_par(c) {
            stack.push(c);
        } else {
            let mb_top = stack.pop();
            let ok = match mb_top {
                Some(top) => get_closing_par(top) == c,
                None => false,
            };
            if !ok {
                return match c {
                    ')' => 3,
                    ']' => 57,
                    '}' => 1197,
                    '>' => 25137,
                     _ => panic!("Not a parenthesis")
                }
            };
        }
    }

    0
}

fn main()
{
    let mut score = 0;
    loop {
        let mut line = String::new();
        let b = read_line_must(&mut line);
        
        if b == 0 {
            break;
        }

        score += corruption_score(line.trim());
    }
    println!("Corruption score: {}", score);
}
