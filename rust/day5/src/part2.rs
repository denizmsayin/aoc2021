use std::io;
use std::cmp;
use std::vec::Vec;
use std::collections::HashSet;

// Simply marking a 2D map would've been much easier... Boi!
// Instead, I got into pairwise checking each line segment for
// intersections. There's a lot of case analysis when diagonals
// are included! Although it should be much faster than marking the map.

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

// Multiplies by 2 for intersection tricks
fn parse_point_must(s: &str) -> Point
{
    let vec: Vec<&str> = s.trim().split(',').collect();
    assert!(vec.len() == 2);
    Point { 
        x: vec[0].parse().unwrap(), 
        y: vec[1].parse().unwrap(), 
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

    if (s.x > e.x && s.y > e.y) || (s.x > e.x && s.y < e.y) {
        return Some(Seg {s: e, e: s}); // Left to right for diagonals too
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

#[derive(PartialEq)]
enum Orientation {
    Vertical,
    Horizontal,
    PosDiag,
    NegDiag
}

fn seg_contains(s: &Seg, p: &Point) -> bool
{
    match get_orientation(&s) {
        Orientation::NegDiag => s.s.x <= p.x && p.x <= s.e.x && s.e.y <= p.y && p.y <= s.s.y,
        _ => s.s.x <= p.x && p.x <= s.e.x && s.s.y <= p.y && p.y <= s.e.y,
    }
}

fn get_orientation(s: &Seg) -> Orientation
{
    if s.s.x == s.e.x {
        return Orientation::Vertical;
    } else if s.s.y == s.e.y {
        return Orientation::Horizontal;
    } else if s.s.x < s.e.x && s.s.y < s.e.y {
        return Orientation::PosDiag;
    } else {
        return Orientation::NegDiag;
    }
}

fn is_diag(o: &Orientation) -> bool
{
    match o {
        Orientation::PosDiag => true,
        Orientation::NegDiag => true,
        _ => false
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
    let s1_o = get_orientation(&s1);
    let s2_o = get_orientation(&s2);

    if !is_diag(&s1_o) && !is_diag(&s2_o) {
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

        return None;
    } else if is_diag(&s1_o) && is_diag(&s2_o) {
        if s1_o == s2_o {
            if s1.s.x <= s2.e.x && s2.s.x <= s1.e.x { // Could be intersecting
                // Ensure intersection here
                if (matches!(s1_o, Orientation::PosDiag) && s1.s.y - s1.s.x == s2.s.y - s2.s.x)  
                   || (matches!(s1_o, Orientation::NegDiag) && s1.s.y + s1.s.x == s2.s.y + s2.s.x)
                {
                    let y_incr = if matches!(s1_o, Orientation::PosDiag) { 1 } else { -1 };
                    let (xs, ys) = if s1.s.x > s2.s.x { (s1.s.x, s1.s.y) } else { (s2.s.x, s2.s.y) };
                    let mut ys = ys;
                    let xe = cmp::min(s1.e.x, s2.e.x) + 1;
                    let mut points = Vec::new();
                    for x in xs..xe {
                        points.push(Point {x, y: ys});
                        ys += y_incr;
                    }
                    return Some(points);
                }
            }
        } else {
            // Different diagonal orientations, one point intersection, maybe
            // Assume lines are y = x + a and y = -x + b
            // Then, the intersection point is x = (b - a) / 2, y = (b + a) / 2
            // Not sure if I sure ignore points having the half coordinates...
            // I will first try not ignoring them, by multiplying each coordinate by 2 (STRETCH!)
            
            let (a, b) = 
                if matches!(s1_o, Orientation::PosDiag) {
                    (s1.s.y - s1.s.x, s2.s.y + s2.s.x)
                } else {
                    (s2.s.y - s2.s.x, s1.s.y + s1.s.x)
                };
          
            /*
            let a = a * 2;
            let b = b * 2;
            */

            
            // Dealing with half points cleanly is hard here, get hacky
            // and add a large constant
            if (a + b) % 2 == 1 {
                return None;
            }

            let tentative_point = Point { x: (b - a) / 2, y: (b + a) / 2 };

            if seg_contains(&s1, &tentative_point) && seg_contains(&s2, &tentative_point) {
                return Some(vec![tentative_point]);
            }
        }
    } else { // Diagonal and vert/horiz intersection
        // Amazing case analysis 
        let (diag, other) = if is_diag(&s1_o) { (s1, s2) } else { (s2, s1) };
        let diag_o = get_orientation(diag);
        let other_o = get_orientation(other);

        let (x, y) = 
            if matches!(diag_o, Orientation::PosDiag) {
                if matches!(other_o, Orientation::Vertical) {
                    (other.s.x, other.s.x + diag.s.y - diag.s.x)
                } else {
                    (other.s.y + diag.s.x - diag.s.y, other.s.y)
                }
            } else {
                if matches!(other_o, Orientation::Vertical) {
                    (other.s.x, diag.s.y + diag.s.x - other.s.x)
                } else {
                    (diag.s.x + diag.s.y - other.s.y, other.s.y)
                }
            };
        
        let tentative_point = Point {x, y};

        if seg_contains(&s1, &tentative_point) && seg_contains(&s2, &tentative_point) {
            return Some(vec![tentative_point]);
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

    // segs.retain(|s| !matches!(get_orientation(&s), Orientation::Diagonal));

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

    /*
    for p in intersection_points {
        print_point(&p);
    }
    */

}
