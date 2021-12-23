use std::io;
use std::cmp::min;
use std::cmp::max;

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

struct Step {
    on: bool,
    xrange: (i64, i64),
    yrange: (i64, i64),
    zrange: (i64, i64)
}

fn parse_coord(s: &str) -> (i64, i64) {
    let v: Vec<&str> = s[2..].split("..").collect();
    assert!(v.len() == 2);
    (v[0].parse().unwrap(), v[1].parse().unwrap())
}

fn parse_step(s: &str) -> Step {
    let on = s.starts_with("on");
    let coords = s.trim().split(' ').last().unwrap();
    let vec: Vec<&str> = coords.split(',').collect();
    assert!(vec.len() == 3);
    let xrange = parse_coord(vec[0]);
    let yrange = parse_coord(vec[1]);
    let zrange = parse_coord(vec[2]);
    Step { on, xrange, yrange, zrange }
}

fn main()
{
    let mut grid = [[[false; 101]; 101]; 101];
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
        if bytes == 0 {
            break;
        }
        let step = parse_step(&line);
        for x in max(-50, step.xrange.0)..min(50, step.xrange.1) + 1 {
            for y in max(-50, step.yrange.0)..min(50, step.yrange.1) + 1 {
                for z in max(-50, step.zrange.0)..min(50, step.zrange.1) + 1 {
                    let i = (x + 50) as usize;
                    let j = (y + 50) as usize;
                    let k = (z + 50) as usize;
                    grid[i][j][k] = step.on;
                }
            }
        }
    }

    let on_cubes = grid.concat().concat().iter().filter(|x| **x).count();
    println!("{}", on_cubes);
}
