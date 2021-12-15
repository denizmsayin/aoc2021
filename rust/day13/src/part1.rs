use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_line(s: &str) -> (i64, i64)
{
    let v: Vec<&str> = s.trim().split(',').collect();
    assert!(v.len() == 2);
    (v[0].parse().unwrap(), v[1].parse().unwrap())
}

fn read_points() -> Vec<(i64, i64)>
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

fn parse_fold(s: &str) -> (bool, i64)
{
    let v: Vec<&str> = s.trim().strip_prefix("fold along ").unwrap().split('=').collect();
    assert!(v.len() == 2);
    (v[0] == "x", v[1].parse().unwrap())
}

fn read_folds() -> Vec<(bool, i64)>
{
    let mut v: Vec<(bool, i64)> = Vec::new();
    loop {
        let mut line = String::new();
        
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            return v;
        }

        let tup = parse_fold(&line);
        v.push(tup);
    }
}

fn main()
{
    let mut pts = read_points();
    let folds = read_folds();
    let (x_axis, line) = *folds.iter().next().unwrap(); 
    
    for (x, y) in pts.iter_mut() {
        let r = if x_axis { x } else { y };
        if *r > line {
            let diff = *r - line;
            *r = line - diff;
        }
    }

    pts.sort();
    pts.dedup();
    println!("{}", pts.len());
}

