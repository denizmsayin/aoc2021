use std::io;
use std::vec::Vec;
use std::collections::HashSet;

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

fn visualize(v: &Vec<(i64, i64)>)
{
    let x_max = v.iter().max_by_key(|c| c.0).unwrap().0;
    let y_max = v.iter().max_by_key(|c| c.1).unwrap().1;
    let c_set: HashSet<_> = HashSet::from_iter(v.iter());
    for i in 0..y_max + 1 {
        for j in 0..x_max + 1 {
            if c_set.contains(&(j, i)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn main()
{
    let mut pts = read_points();
    let folds = read_folds();
    
    for (x_axis, line) in folds.iter() {
        for (x, y) in pts.iter_mut() {
            let r = if *x_axis { x } else { y };
            if *r > *line {
                let diff = *r - *line;
                *r = *line - diff;
            }
        }
    }

    pts.sort();
    pts.dedup();

    println!("Points: {}", pts.len());

    visualize(&pts);
}

