use std::io;
use std::vec::Vec;
use std::collections::HashMap;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_rule(s: &str) -> ((char, char), char)
{
    let v: Vec<&str> = s.trim().split(" -> ").collect();
    assert!(v.len() == 2);
    let c: Vec<char> = v[0].chars().collect();
    ((c[0], c[1]), v[1].chars().next().unwrap())
}

fn read_input() -> (Vec<char>, HashMap<(char, char), char>)
{
    let mut line = String::new();
    let mut m: HashMap<(char, char), char> = HashMap::new();
    
    read_line_must(&mut line);
    let start: Vec<char> = line.trim().chars().collect();

    read_line_must(&mut String::new());

    loop {
        let mut line = String::new();

        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            return (start, m);
        }

        let (k, v) = parse_rule(&line);
        m.insert(k, v);
    }
}

fn main()
{
    let (mut seq, rules) = read_input();
    
    for step in 1..11 {
        let mut next: Vec<char> = Vec::new();

        for (c, nc) in seq.iter().zip(seq.iter().skip(1)) {
            let b = rules[&(*c, *nc)];
            next.push(*c);
            next.push(b);
        }
        next.push(*seq.last().unwrap());

        seq = next;
        println!("{}", step);
    }

    let mut ctr: HashMap<char, usize> = HashMap::new();
    for c in seq.iter() {
        let count = ctr.entry(*c).or_insert(0);
        *count += 1;
    }
    
    let min_c = ctr.values().min().unwrap();
    let max_c = ctr.values().max().unwrap();

    println!("{} - {} = {}", max_c, min_c, max_c - min_c);
}

