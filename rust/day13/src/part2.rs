use std::io;
use std::vec::Vec;
use std::collections::HashMap;
use std::collections::HashSet;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_line(s: &str) -> (i64, i64)
{
    let v: Vec<&str> = s.trim().split(',').collect();
    println!("{}", v.len());
    assert!(v.len() == 2);
    (v[0].parse().unwrap(), v[1].parse().unwrap())
}

fn read_segments() -> Vec<(i64, i64)>
{
    let mut v: Vec<(i64, i64)> = Vec::new();
    loop {
        let mut line = String::new();
        
        let bytes = read_line_must(&mut line);

        if bytes == 0 || line.trim() == "" {
            return v;
        }

        let tup = parse_line(&line);
        v.push(tup);
    }
}

fn main()
{
    let mut segs = read_segments();
    
    for seg in segs.iter() {
        println!("{:?}", seg);
    }

}

