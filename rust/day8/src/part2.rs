use std::io;
use std::vec::Vec;

// This part is much more interesting!
// Still, it's not too complicated to derive some nice rules
// for disambiguation. Finding the number of common characters
// with 1 and 4 seems to be easy enough.

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_code(s: &str) -> Vec<u8> 
{
    let mut bs = s.as_bytes().to_vec();
    bs.sort();
    bs
}

// Can be made O(n) since codes are sorted, but
// gonna go with O(n^2) since they're short anyway
fn count_common(v1: &Vec<u8>, v2: &Vec<u8>) -> usize
{
    let mut c = 0;
    for a in v1 {
        for b in v2 {
            if a == b {
                c += 1;
                break;
            }
        }
    }
    c
}

fn lookup_code(codebook: &[Vec<u8>; 10], code: &Vec<u8>) -> usize
{
    for i in 0..10 {
        if code == &codebook[i] {
            return i;
        }
    }
    panic!("Failed to lookup code");
}

fn main()
{
    let mut total = 0;
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            break;
        }

        let sides: Vec<&str> = line.trim().split('|').collect();
        assert!(sides.len() == 2);

        let patterns: Vec<Vec<u8>> = sides[0].trim()
            .split_ascii_whitespace()
            .map(parse_code)
            .collect();

        // Kind of lame to initialize this with empty vectors, but oh well!
        // Making this one proper requires partial building and then finalization,
        // which is a whole lot of effort.
        let mut codebook: [Vec<u8>; 10] = Default::default();
       
        // First pass for finding 1, 4, 7, 8
        for pat in patterns.iter() {
            match pat.len() {
                2 => codebook[1] = pat.clone(),
                3 => codebook[7] = pat.clone(),
                4 => codebook[4] = pat.clone(),
                7 => codebook[8] = pat.clone(),
                _ => ()
            };
        }

        // Second pass for finding the rest
        for pat in patterns.iter() {
            let n = pat.len();
            if n == 5 || n == 6 {
                let com1 = count_common(&pat, &codebook[1]);
                let com4 = count_common(&pat, &codebook[4]);
                if n == 5 {
                    if com1 == 1 && com4 == 2 {
                        codebook[2] = pat.clone();
                    } else if com1 == 2 && com4 == 3 {
                        codebook[3] = pat.clone();
                    } else if com1 == 1 && com4 == 3 {
                        codebook[5] = pat.clone();
                    } else {
                        panic!("Unexpected common counts!");
                    }
                } else {
                    if com1 == 2 && com4 == 3 {
                        codebook[0] = pat.clone();
                    } else if com1 == 1 && com4 == 3 {
                        codebook[6] = pat.clone();
                    } else if com1 == 2 && com4 == 4 {
                        codebook[9] = pat.clone();
                    } else {
                        panic!("Unexpected common counts!");
                    }
                }
            }
        }

        // Now, use the complete codebook for decoding the digits
        let mut value = 0;
        for code in sides[1].trim().split_ascii_whitespace().map(parse_code) {
            let d = lookup_code(&codebook, &code);
            value = 10 * value + d;
        }
        println!("Value: {}", value);

        total += value;
    }
    println!("Total: {}", total);
}
