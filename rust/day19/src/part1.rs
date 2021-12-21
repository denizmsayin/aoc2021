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
struct Rotation(I3, I3, I3);

#[derive(Debug,Clone)]
struct Transl(I3);

#[derive(Debug,Clone)]
struct TranslRot {
    rot: Rotation,
    t: Transl
}

trait Transform {
    fn trans(&self, p: &I3) -> I3;
    fn inv(&self) -> Self;
    fn comp(&self, o: &Self) -> Self;
}

impl Transform for Rotation {
    // Need to avoid arbitrary 3x3 matrix manually, since there are no checks...
    fn trans(&self, p: &I3) -> I3 {
        I3(dot(&self.0, p), dot(&self.1, p), dot(&self.2, p))
    }

    fn inv(&self) -> Self {
        let t = self;
        Rotation(I3(t.0.0, t.1.0, t.2.0),
                 I3(t.0.1, t.1.1, t.2.1),
                 I3(t.0.2, t.1.2, t.2.2))
    }

    fn comp(&self, o: &Self) -> Self {
        fn transpose(t: &Rotation) -> Rotation {
            Rotation(I3(t.0.0, t.1.0, t.2.0),
                     I3(t.0.1, t.1.1, t.2.1),
                     I3(t.0.2, t.1.2, t.2.2))
        }

        let ot = transpose(o);
        Rotation(I3(dot(&self.0, &ot.0), dot(&self.0, &ot.1), dot(&self.0, &ot.2)),
                 I3(dot(&self.1, &ot.0), dot(&self.1, &ot.1), dot(&self.1, &ot.2)),
                 I3(dot(&self.2, &ot.0), dot(&self.2, &ot.1), dot(&self.2, &ot.2)))
    }
}

impl Transform for Transl {
    fn trans(&self, p: &I3) -> I3 {
        let t = &self.0;
        I3(t.0 + p.0, t.1 + p.1, t.2 + p.2)
    }

    fn inv(&self) -> Self {
        let t = &self.0;
        Transl(I3(-t.0, -t.1, -t.2))
    }

    fn comp(&self, o: &Self) -> Self {
        Transl(self.trans(&o.0))
    }
}

impl Transform for TranslRot {
    fn trans(&self, p: &I3) -> I3 {
        self.t.trans(&self.rot.trans(p))
    }

    fn inv(&self) -> Self {
        let rinv = self.rot.inv();
        let tinv = self.t.inv();
        let t = Transl(rinv.trans(&tinv.0));
        TranslRot { rot: rinv, t }
    }

    fn comp(&self, o: &Self) -> Self {
        let rot = self.rot.comp(&o.rot);
        let r0t1 = self.rot.trans(&o.t.0);
        let t = Transl(Transl(r0t1).trans(&self.t.0));
        TranslRot { rot, t }
    }
}

fn dot(a: &I3, b: &I3) -> i64 {
    a.0 * b.0 + a.1 * b.1 + a.2 * b.2
}

const UNIT: Rotation = Rotation(I3(1, 0, 0),
                                I3(0, 1, 0),
                                I3(0, 0, 1));
 
const ROT_X: Rotation = Rotation(I3(1, 0, 0),
                                 I3(0, 0, -1),
                                 I3(0, 1, 0));

const ROT_Y: Rotation = Rotation(I3(0, 0, 1),
                                 I3(0, 1, 0),
                                 I3(-1, 0, 0));

const ROT_Z: Rotation = Rotation(I3(0, -1, 0),
                                 I3(1, 0, 0),
                                 I3(0, 0, 1));

fn gen_face_transforms() -> Vec<Rotation> {
    fn gut(t: &Rotation) -> [Rotation; 4] {
        // Generate transforms for rotating up vector
        let r1 = ROT_X.comp(t);        
        let r2 = ROT_X.comp(&r1);
        let r3 = ROT_X.comp(&r2);
        [ (*t).clone(), r1, r2, r3 ]
    }

    let tf0 = UNIT;
    let tf1 = ROT_Y;
    let tf2 = ROT_Y.comp(&tf1);
    let tf3 = ROT_Y.comp(&tf2);
    let tf4 = ROT_Z;
    let tf5 = ROT_Z.comp(&ROT_Z).comp(&tf4);

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

fn match_scanners(s1: &[I3], s0: &[I3], rots: &[Rotation]) -> Option<TranslRot> {
    let mut max_match = 0;
    let mut best_params = None;
    for rot in rots.iter() {
        let s1_rot: Vec<I3> = s1.iter().map(|p| rot.trans(&p)).collect();
        for i in 0..s0.len() {
            for j in 0..s1.len() {
                let tv = Transl(Transl(s1_rot[j].clone()).inv().trans(&s0[i]));
                let s1_tsd: Vec<I3> = s1_rot.iter().map(|p| tv.trans(&p)).collect();
                let match_count = count_equal(&s0, &s1_tsd);
                if match_count >= 12 && match_count > max_match {
                    max_match = match_count;
                    best_params = Some(TranslRot { rot: (*rot).clone(), t: tv });
                }
            }
        }
    }
    best_params
}

fn generate_to_0_transforms(tfs: &HashMap<(usize, usize), TranslRot>) -> Vec<TranslRot> {
    Vec::new()
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
                println!("{:?}", etf.inv().comp(&etf));
                scanner_tfs.insert((j, i), etf.inv());
                scanner_tfs.insert((i, j), etf);
            }
        }
    }

    for (p, tf) in scanner_tfs.iter() {
        println!("{:?} {:?}", p, tf);
    }
    

//     for s in scanners.iter() {
//         println!("{:?}", s);
//     }
}
