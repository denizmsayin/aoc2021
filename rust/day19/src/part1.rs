use std::io;
use std::fmt;
use std::vec::Vec;
use std::collections::HashMap;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct I3(i64, i64, i64);

#[derive(Debug,Clone)]
struct Tf(I3, I3, I3);

trait Ptf {
    fn transform(&self, p: &I3) -> I3;
    fn invert(&self, p: &I3) -> I3;
}

fn add(a: &I3, b: &I3) -> I3 {
    I3(a.0 + b.0, a.1 + b.1, a.2 + b.2)
}

fn sub(a: &I3, b: &I3) -> I3 {
    I3(a.0 - b.0, a.1 - b.1, a.2 - b.2)
}

fn dot(a: &I3, b: &I3) -> i64 {
    a.0 * b.0 + a.1 * b.1 + a.2 * b.2
}

fn trans(t: &Tf, a: &I3) -> I3 {
    I3(dot(&t.0, a), dot(&t.1, a), dot(&t.2, a))
}

fn transpose(t: &Tf) -> Tf {
    Tf(I3(t.0.0, t.1.0, t.2.0),
       I3(t.0.1, t.1.1, t.2.1),
       I3(t.0.2, t.1.2, t.2.2))
}

fn comp(t0: &Tf, t1: &Tf) -> Tf {
    let t1t = transpose(t1);
    Tf(I3(dot(&t0.0, &t1t.0), dot(&t0.0, &t1t.1), dot(&t0.0, &t1t.2)),
       I3(dot(&t0.1, &t1t.0), dot(&t0.1, &t1t.1), dot(&t0.1, &t1t.2)),
       I3(dot(&t0.2, &t1t.0), dot(&t0.2, &t1t.1), dot(&t0.2, &t1t.2)))
}

const UNIT: Tf = Tf(I3(1, 0, 0),
                    I3(0, 1, 0),
                    I3(0, 0, 1));

const ROT_X: Tf = Tf(I3(1, 0, 0),
                     I3(0, 0, -1),
                     I3(0, 1, 0));

const ROT_Y: Tf = Tf(I3(0, 0, 1),
                     I3(0, 1, 0),
                     I3(-1, 0, 0));

const ROT_Z: Tf = Tf(I3(0, -1, 0),
                     I3(1, 0, 0),
                     I3(0, 0, 1));

fn gen_face_transforms() -> Vec<Tf> {
    fn gut(t: &Tf) -> [Tf; 4] {
        // Generate transforms for rotating up vector
        let r1 = comp(&ROT_X, t);        
        let r2 = comp(&ROT_X, &r1);        
        let r3 = comp(&ROT_X, &r2);        
        [ (*t).clone(), r1, r2, r3 ]
    }

    let tf0 = UNIT;
    let tf1 = ROT_Y;
    let tf2 = comp(&ROT_Y, &tf1);
    let tf3 = comp(&ROT_Y, &tf2);
    let tf4 = ROT_Z;
    let tf5 = comp(&ROT_Z, &comp(&ROT_Z, &tf4));

    [ gut(&tf0), gut(&tf1), gut(&tf2), gut(&tf3), gut(&tf4), gut(&tf5) ].concat()
}

fn parse_pt(s: &str) -> I3 {
    let v: Vec<i64> = s.trim().split(",").map(|p| p.parse().unwrap()).collect();
    assert!(v.len() == 3);
    I3(v[0], v[1], v[2])
}

fn read_scanner() -> Option<Vec<I3>> {
    let mut pts = Vec::new();
    read_line_must(&mut String::new());
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);

        if bytes == 0 || line.trim() == "" {
            break;
        }

        pts.push(parse_pt(&line));
    }
    if pts.is_empty() { None } else { Some(pts) }
}

fn count_equal(s0: &[I3], s1: &[I3]) -> usize {
    let mut cnt = 0;
    for p0 in s0.iter() {
        for p1 in s1.iter() {
            if p0 == p1 {
                cnt += 1;
            }
        }
    }
    cnt
}

struct ExTf {
    r: Tf,
    t: I3
}

fn match_scanners(s0: &[I3], s1: &[I3], tfs: &[Tf]) -> Option<ExTf> {
    let mut max_match = 0;
    let mut best_params = None;
    for tf in tfs.iter() {
        let s1_tfd: Vec<I3> = s1.iter().map(|p| trans(&tf, &p)).collect();
        for i in 0..s0.len() {
            for j in 0..s1.len() {
                let tv = sub(&s0[i], &s1_tfd[j]);
                let s1_tsd: Vec<I3> = s1_tfd.iter().map(|p| add(&p, &tv)).collect();
                let match_count = count_equal(&s0, &s1_tsd);
                if match_count >= 12 && match_count > max_match {
                    max_match = match_count;
                    best_params = Some(ExTf { r: (*tf).clone(), t: tv });
                }
            }
        }
    }
    best_params
}

fn main()
{
    let face_tfs = gen_face_transforms();
    let mut scanner_tfs = HashMap::new();

    let mut scanners = Vec::new();
    while let Some(s) = read_scanner() {
        scanners.push(s);
    }

    for i in 0..scanners.len() {
        for j in i+1..scanners.len() {
            if let Some(etf) = match_scanners(&scanners[i], &scanners[j], &face_tfs) {
                println!("Matched {} -> {}", i, j);
                scanner_tfs.insert((i, j), etf);
            }
        }
    }

    

//     for s in scanners.iter() {
//         println!("{:?}", s);
//     }
}
