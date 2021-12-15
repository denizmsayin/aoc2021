use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_line(s: &str) -> Vec<u8>
{
    let mut v = Vec::new();
    for c in s.trim().as_bytes() {
        v.push(*c - 48);
    }
    v
}

fn read_octmap() -> Vec<Vec<u8>>
{
    let mut octmap: Vec<Vec<u8>> = Vec::new();
    
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            break;
        }

        octmap.push(parse_line(&line));
    }

    octmap
}

const OFFS: [(i64, i64); 8] = [(1, 0), (1, 1), (0, 1), (-1, 1), 
                               (-1, 0), (-1, -1), (0, -1), (1, -1)];

fn main()
{
    let mut octmap = read_octmap();
    let m = octmap.len() as i64;
    let n = octmap[0].len() as i64;

    let mut step = 1;
    loop {
        let mut flash_stack: Vec<(i64, i64)> = Vec::new();
        let mut flashed: Vec<Vec<bool>> = vec![vec![false; octmap.len()]; octmap[0].len()];

        // First pass, increase by one and note flashers
        for i in 0..m {
            for j in 0..n {
                let ip = i as usize;
                let jp = j as usize;
                octmap[ip][jp] += 1;
                if octmap[ip][jp] > 9 { // flash!
                    flash_stack.push((i, j));
                    flashed[ip][jp] = true;
                }
            }
        }

        // Now, recursively apply flashes using a stack
        while let Some((i, j)) = flash_stack.pop() {
            for (ioff, joff) in OFFS.iter() {
                let ii = i + ioff;
                let jj = j + joff;
                if 0 <= ii && ii < m && 0 <= jj && jj < n {
                    let iip = ii as usize;
                    let jjp = jj as usize;
                    if !flashed[iip][jjp] {
                        octmap[iip][jjp] += 1;
                        if octmap[iip][jjp] > 9 {
                            flash_stack.push((ii, jj));
                            flashed[iip][jjp] = true;
                        }
                    }
                }
            }
        }

        // Zero flashed positions and add to count, could use
        // another data structure to hold those but let's just
        // do a quick loop over the whole octmap.
        let mut all_flashed = true;
        for i in 0..m as usize {
            for j in 0..n as usize {
                if flashed[i][j] {
                    octmap[i][j] = 0;
                } else {
                    all_flashed = false;
                }
            }
        }

        println!("Step {}", step);
        if all_flashed {
            println!("All the octopi flashed!");
            break;
        }
        
        step += 1;
    }

}
