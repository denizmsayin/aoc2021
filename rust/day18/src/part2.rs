use std::io;
use std::fmt;
use std::vec::Vec;

// An overengineered, overperformant first try for the solution.
// Has some subtle mistakes: some divisors are missed due to an
// obviously bad algorithm... One more step is necessary after
// gathering prime divisors, but forget it!

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

#[derive(Debug,Clone)]
enum Elem {
    Number(u64),
    Pair(Box<Pair>),
}

#[derive(Debug,Clone)]
struct Pair {
    l: Elem,
    r: Elem
}

impl fmt::Display for Elem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Elem::Number(x) => write!(f, "{}", x),
            Elem::Pair(p) => write!(f, "{}", p)
        }
    }
}
    

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{},{}]", self.l, self.r)
    }
}

fn parse_digit(s: &[u8]) -> (usize, u64)
{
    let c = s[0];
    assert!('0' as u8 <= c && c <= '9' as u8);
    (1, (c - '0' as u8) as u64)
}

fn parse_elem(s: &[u8]) -> (usize, Elem)
{
    if s[0] == '[' as u8 {
        let (size, pair) = parse_pair(s);
        (size, Elem::Pair(Box::new(pair)))
    } else {
        let (size, num) = parse_digit(s);
        (size, Elem::Number(num))
    }
}

fn parse_pair(s: &[u8]) -> (usize, Pair) {
    assert!(s[0] == '[' as u8);
    let (first_size, first_elem) = parse_elem(&s[1..]);
    assert!(s[1+first_size] == ',' as u8);
    let (second_size, second_elem) = parse_elem(&s[2+first_size..]);
    assert!(s[2 + first_size + second_size] == ']' as u8);
    (3 + first_size + second_size, Pair { l: first_elem, r: second_elem })
}

fn parse_line(s: &str) -> Pair { 
    parse_pair(s.as_bytes()).1 
}

fn get_leaf(p: &mut Pair, n: usize) -> Option<&mut u64> {
    enum LeafSearch<'a> {
        Leaf(&'a mut u64),
        Count(usize)
    }

    use LeafSearch::Count;
    use LeafSearch::Leaf;

    fn fp(p: &mut Pair, n: usize) -> LeafSearch<'_> {
        match fe(&mut p.l, n) {
            l@Leaf(_) => l,
            Count(left_leaves) => {
                match fe(&mut p.r, n - left_leaves) {
                    l@Leaf(_) => l,
                    Count(right_leaves) => Count(left_leaves + right_leaves)
                }
            }
        }
    }

    fn fe(e: &mut Elem, n: usize) -> LeafSearch<'_> {
        match e {
            Elem::Pair(p) => fp(p, n),
            Elem::Number(r) => if n == 0 { Leaf(r) } else { Count(1) }
        }
    }

    match fp(p, n) {
        Leaf(r) => Some(r),
        Count(_) => None
    }
}

fn explode(p: &mut Pair) -> bool {
    
    fn unpack_two_num_pair(p: &Pair) -> Option<(u64, u64)> {
        match p.l {
            Elem::Number(x) => 
                match p.r {
                    Elem::Number(y) => Some((x, y)),
                    _ => None 
                }
            _ => None
        }
    }

    enum BoomSearch {
        Boom(usize, u64, u64),
        Count(usize)
    }

    use BoomSearch::Count;
    use BoomSearch::Boom;

    fn search_p(p: &mut Pair, d: u64) -> BoomSearch {
        match search_e(&mut p.l, d) {
            b@Boom(_,_,_) => b,
            Count(left_leaves) => {
                match search_e(&mut p.r, d) {
                    Boom(i, l, r) => Boom(left_leaves + i, l, r),
                    Count(right_leaves) => Count(left_leaves + right_leaves),
                }
            }
        }
    }

    fn search_e(e: &mut Elem, d: u64) -> BoomSearch {
        if let Elem::Pair(p) = e {
            if let (4, Some((x, y))) = (d, unpack_two_num_pair(p)) {
                *e = Elem::Number(0);
                Boom(0, x, y)
            } else {
                search_p(p, d + 1)
            }
        } else {
            Count(1)
        }
    } 

    match search_p(p, 1) {
        Count(_) => false,
        Boom(i, l, r) => {
            if i > 0 {
                if let Some(left_leaf) = get_leaf(p, i - 1) {
                    *left_leaf += l;
                }
            }
            if let Some(right_leaf) = get_leaf(p, i + 1) {
                *right_leaf += r;
            }
            true
        }
    }
}

fn split(p: &mut Pair) -> bool {
    
    fn search_p(p: &mut Pair) -> bool {
        let done = search_e(&mut p.l);
        if done {
            true
        } else {
            search_e(&mut p.r)
        }
    }

    fn search_e(e: &mut Elem) -> bool {
        match e {
            Elem::Number(x) => {
                let x = *x;
                if x >= 10 {
                    let l = x / 2;
                    let r = x - l;
                    *e = Elem::Pair(Box::new(Pair { l: Elem::Number(l), r: Elem::Number(r)}));
                    true
                } else {
                    false
                }
            },
            Elem::Pair(p) => {
                search_p(p)
            }
        }
    }

    search_p(p)
}

fn reduce(p: &mut Pair) {
    let boom = explode(p);
    if boom {
        reduce(p);
    } else {
        let cut = split(p);
        if cut {
            reduce(p);
        }
    }
}

fn add(p1: &Pair, p2: &Pair) -> Pair {
    let p1 = (*p1).clone();
    let p2 = (*p2).clone();
    let mut p3 = Pair { l: Elem::Pair(Box::new(p1)), 
                        r: Elem::Pair(Box::new(p2)) };
    reduce(&mut p3);
    p3
}

fn mag(p: &Pair) -> u64 {

    fn mag_e(e: &Elem) -> u64 {
        match e {
            Elem::Number(x) => *x,
            Elem::Pair(p) => mag(p)
        }
    }

    3 * mag_e(&p.l) + 2 * mag_e(&p.r)
}

fn main()
{
    let mut nums = Vec::new();
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
        
        if bytes == 0 {
            break;
        }

        nums.push(parse_line(&line));
    }

    let mut max_mag = 0;
    for i in 0..nums.len() {
        for j in 1..nums.len() {
            let m = mag(&add(&nums[i], &nums[j]));
            if m > max_mag {
                max_mag = m;
            }
            let m = mag(&add(&nums[j], &nums[i]));
            if m > max_mag {
                max_mag = m;
            }
        }
    }

    println!("{}", max_mag);
}
