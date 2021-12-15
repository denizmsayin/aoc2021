use std::io;
use std::cmp;
use std::vec::Vec;
use std::collections::HashSet;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

#[derive(PartialEq,Eq,Hash)]
struct Point {
    x: i64,
    y: i64
}

struct Seg {
    s: Point,
    e: Point
}

fn parse_point_must(s: &str) -> Point
{
    let vec: Vec<&str> = s.trim().split(',').collect();
    assert!(vec.len() == 2);
    Point { 
        x: vec[0].parse().unwrap(), 
        y: vec[1].parse().unwrap() 
    }
}

fn read_seg() -> Option<Seg>
{
    let mut line = String::new();

    let bytes = read_line_must(&mut line);

    if bytes == 0 {
        return None;
    }

    let vec: Vec<&str> = line.trim().split("->").collect();
    
    assert!(vec.len() == 2);

    let s = parse_point_must(vec[0]);
    let e = parse_point_must(vec[1]);

    if (s.x == e.x && e.y < s.y) || (s.y == e.y && e.x < s.x) {
        return Some(Seg {s: e, e: s}); // Swap so that min is at s always
    }

    Some(Seg {s, e})
}

#[derive(PartialEq)]
enum Axis {
    X,
    Y
}

fn get_axis(p: &Point, a: &Axis) -> i64
{
    match a {
        Axis::X => p.x,
        Axis::Y => p.y
    }
}

// (varying_axis, constant_axis)
fn get_axis_info(s: &Seg) -> (Axis, Axis)
{
    if s.s.x == s.e.x {
        return (Axis::Y, Axis::X);
    } else if s.s.y == s.e.y {
        return (Axis::X, Axis::Y);
    } else {
        panic!("Found diagonal line, no!");
    }
}

enum Orientation {
    Vertical,
    Horizontal,
    Diagonal
}

fn get_orientation(s: &Seg) -> Orientation
{
    if s.s.x == s.e.x {
        return Orientation::Vertical;
    } else if s.s.y == s.e.y {
        return Orientation::Horizontal;
    } else {
        return Orientation::Diagonal;
    }
}

fn point_from_axis_values(a1: &Axis, v1: i64, v2: i64) -> Point
{
    match a1 {
        Axis::X => Point {x: v1, y: v2},
        Axis::Y => Point {x: v2, y: v1}
    }
}

fn find_intersections(s1: &Seg, s2: &Seg) -> Option<Vec<Point>>
{
    let (s1_va, s1_ca) = get_axis_info(s1);
    let (s2_va, s2_ca) = get_axis_info(s2);

    if s1_va == s2_va { // Same orientation
        // Check if on the same horizontal/vertical line
        let s1_c = get_axis(&s1.s, &s1_ca);
        let s2_c = get_axis(&s2.s, &s2_ca);
        if s1_c == s2_c {
            // Single line intersection test
            let a1 = get_axis(&s1.s, &s1_va);
            let a2 = get_axis(&s1.e, &s1_va);
            let b1 = get_axis(&s2.s, &s2_va);
            let b2 = get_axis(&s2.e, &s2_va);
            if a1 <= b2 && b1 <= a2 {
                let mut points = Vec::new();
                // Intersects, gotta traverse the points
                let s = cmp::max(a1, b1);
                let e = cmp::min(a2, b2) + 1;
                for i in s..e {
                    points.push(point_from_axis_values(&s1_va, i, s1_c)); 
                }
                return Some(points);
            }
        }
    } else { // Perpendicular, a simpler case
        let s1_vs = get_axis(&s1.s, &s1_va);
        let s1_ve = get_axis(&s1.e, &s1_va);
        let s2_vs = get_axis(&s2.s, &s2_va);
        let s2_ve = get_axis(&s2.e, &s2_va);
        let s1_c = get_axis(&s1.s, &s1_ca);
        let s2_c = get_axis(&s2.s, &s2_ca);
        if s1_vs <= s2_c && s2_c <= s1_ve && s2_vs <= s1_c && s1_c <= s2_ve {
            return Some(vec![point_from_axis_values(&s1_ca, s1_c, s2_c)]);
        }
    }

    None
}

fn main()
{
    let mut segs = Vec::new();

    loop {
        match read_seg() {
            Some(s) => segs.push(s),
            None => break,
        }
    }

    segs.retain(|s| !matches!(get_orientation(&s), Orientation::Diagonal));

    let segs = segs;
    let n = segs.len();
    let mut intersection_points = HashSet::new();

    for i in 0..n {
        for j in (i+1)..n {
            if let Some(points) = find_intersections(&segs[i], &segs[j]) {
                for p in points {
                    intersection_points.insert(p);
                }
            }
        }
    }

    println!("{}", intersection_points.len());

}
