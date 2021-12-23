use std::io;
use std::cmp::min;
use std::cmp::max;

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

#[derive(Clone)]
struct Cuboid {
    xrange: (i64, i64),
    yrange: (i64, i64),
    zrange: (i64, i64)
}

struct Step {
    on: bool,
    cuboid: Cuboid
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
    Step { on, cuboid: Cuboid { xrange, yrange, zrange } }
}

fn intersection_1d(a: (i64, i64), b: (i64, i64)) -> Option<(i64, i64)> {
    if a.0 <= b.1 && b.0 <= a.1 {
        Some((max(a.0, b.0), min(a.1, b.1)))
    } else {
        None
    }
}

fn intersection(s0: &Cuboid, s1: &Cuboid) -> Option<Cuboid> {
    if let Some(xrange) = intersection_1d(s0.xrange, s1.xrange) {
        if let Some(yrange) = intersection_1d(s0.yrange, s1.yrange) {
            if let Some(zrange) = intersection_1d(s0.zrange, s1.zrange) {
                return Some(Cuboid { xrange, yrange, zrange });
            }
        }
    }
    None
}

fn on_points(c: &Cuboid) -> i64 {
    (c.xrange.1 - c.xrange.0 + 1) * (c.yrange.1 - c.yrange.0 + 1) * (c.zrange.1 - c.zrange.0 + 1)
}

fn diff_3d(a: &Cuboid, b: &Cuboid) -> Vec<Cuboid> {
    if let Some(inter) = intersection(a, b) {
        let mut v = Vec::new();
        if a.xrange.0 < inter.xrange.0 {
            v.push(Cuboid { xrange: (a.xrange.0, inter.xrange.0 - 1),
                            yrange: inter.yrange,
                            zrange: inter.zrange });
        }
        if inter.xrange.1 < a.xrange.1 {
            v.push(Cuboid { xrange: (inter.xrange.1 + 1, a.xrange.1),
                            yrange: inter.yrange,
                            zrange: inter.zrange });
        }
        if a.zrange.0 < inter.zrange.0 {
            v.push(Cuboid { xrange: a.xrange,
                            yrange: inter.yrange,
                            zrange: (a.zrange.0, inter.zrange.0 - 1) });
        }
        if inter.zrange.1 < a.zrange.1 {
            v.push(Cuboid { xrange: a.xrange,
                            yrange: inter.yrange,
                            zrange: (inter.zrange.1 + 1, a.zrange.1) });
        }
        if a.yrange.0 < inter.yrange.0 {
            v.push(Cuboid { xrange: a.xrange,
                            yrange: (a.yrange.0, inter.yrange.0 - 1),
                            zrange: a.zrange });
        }
        if inter.yrange.1 < a.yrange.1 {
            v.push(Cuboid { xrange: a.xrange,
                            yrange: (inter.yrange.1 + 1, a.yrange.1),
                            zrange: a.zrange });
        }
        v
    } else {
        vec![a.clone()]
    }
}

fn main()
{
    let mut v = Vec::new();
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
        if bytes == 0 {
            break;
        }

        let step = parse_step(&line);
        let mut next_v = Vec::new();
        for cuboid in v.iter() {
            for part in diff_3d(&cuboid, &step.cuboid) {
                next_v.push(part);
            }
        }
        if step.on {
            next_v.push(step.cuboid);
        }
        v = next_v;
    }

    let on_cubes: i64 = v.iter().map(on_points).sum();
    println!("{}", on_cubes);
}

/*
fn diff_1d(a: (i64, i64), b: (i64, i64)) -> Vec<(i64, i64)> {
    if let Some((is, ie)) = intersection_1d(a, b) {
        let mut v = Vec::new();
        if a.0 < is {
            v.push((a.0, is - 1))
        }
        if ie < a.1 {
            v.push((ie + 1, a.1))
        }
        v
    } else {
        vec![a]
    }
}

#[test]
fn diff_1d_test() {
    assert_eq!(diff_1d((3, 5), (9, 10)), vec![(3, 5)]);
    assert_eq!(diff_1d((3, 5), (-10, 2)), vec![(3, 5)]);
    assert_eq!(diff_1d((3, 5), (4, 5)), vec![(3, 3)]);
    assert_eq!(diff_1d((3, 5), (4, 15)), vec![(3, 3)]);
    assert_eq!(diff_1d((3, 5), (3, 5)), vec![]);
    assert_eq!(diff_1d((3, 5), (0, 15)), vec![]);
    assert_eq!(diff_1d((3, 5), (2, 3)), vec![(4, 5)]);
    assert_eq!(diff_1d((3, 5), (3, 3)), vec![(4, 5)]);
    assert_eq!(diff_1d((3, 5), (4, 4)), vec![(3, 3), (5, 5)]);
}
*/

