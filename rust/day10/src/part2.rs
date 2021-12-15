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
        _ => panic!("Not an opening parenthesis")
    }
}

fn is_opening_par(c: char) -> bool {
    return c == '(' || c == '{' || c == '[' || c == '<';
}

fn get_completer_string(s: &str) -> Option<String>
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
                return None; 
            };
        }
    }

    let mut compl = String::new();
    while let Some(top) = stack.pop() {
        compl.push(get_closing_par(top));
    }

    Some(compl)
}

fn get_syntax_score(compl: &String) -> u64
{
    let mut score = 0;
    for c in compl.chars() {
        score = 5 * score + match c {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => panic!("Not par")
        }
    }
    score
}

fn main()
{
    let mut compls: Vec<String> = Vec::new();
    loop {
        let mut line = String::new();
        let b = read_line_must(&mut line);
        
        if b == 0 {
            break;
        }

         if let Some(compl) = get_completer_string(line.trim()) {
             compls.push(compl);
         }
    }

    compls.sort_by_key(get_syntax_score);

    let mid_compl = &compls[compls.len() / 2];
    println!("Mid score: {}", get_syntax_score(mid_compl));
}
