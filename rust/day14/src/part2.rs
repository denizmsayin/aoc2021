use std::io;
use std::vec::Vec;
use std::cmp;
use std::hash::Hash;
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

fn counter_add<K: Eq + Hash + Copy>(ctr: &mut HashMap<K, i64>, k: &K, c: i64)
{
    let count = ctr.entry(*k).or_insert(0);
    *count += c;
}

fn counter_max<K: Eq + Copy + Hash>(c0: &HashMap<K, i64>, c1: &HashMap<K, i64>) -> HashMap<K, i64>
{
    let mut res: HashMap<K, i64> = HashMap::new();
    for k in c0.keys() {
        res.insert(*k, cmp::max(c0[k], c1[k]));
    }
    res
}

fn read_input() -> (HashMap<(char, char), i64>, HashMap<(char, char), char>)
{
    let mut line = String::new();
    let mut m: HashMap<(char, char), char> = HashMap::new();
    
    read_line_must(&mut line);
    let mut start: HashMap<(char, char), i64> = HashMap::new();
    for (c, nc) in line.trim().chars().zip(line.trim().chars().skip(1)) {
        counter_add(&mut start, &(c, nc), 1);
    }

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

    for _ in 0..40 {
        let mut next: HashMap<(char, char), i64> = HashMap::new();

        for ((c, nc), count) in seq.iter() {
            let b = rules[&(*c, *nc)];
            counter_add(&mut next, &(*c, b), *count);
            counter_add(&mut next, &(b, *nc), *count);
        }

        seq = next;
    }
    
    
    let mut left_ctr: HashMap<char, i64> = HashMap::new();
    let mut right_ctr: HashMap<char, i64> = HashMap::new();
    for ((c, nc), count) in seq.iter() {
        counter_add(&mut left_ctr, c, *count);
        counter_add(&mut right_ctr, nc, *count);
    }

    let char_ctr: HashMap<char, i64> = counter_max(&left_ctr, &right_ctr);

    let min_c = char_ctr.values().min().unwrap();
    let max_c = char_ctr.values().max().unwrap();

    println!("{}", max_c - min_c);
}

