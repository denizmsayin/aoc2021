#![feature(map_first_last)]

// ^ The map_first_last experimental feature is simply too practical to not use here

use std::io;
use std::vec::Vec;
use std::collections::BTreeSet;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_line(s: &str) -> Vec<u32>
{
    let mut v = Vec::new();
    for c in s.trim().as_bytes() {
        v.push((*c - 48) as u32);
    }
    v
}

fn read_2d() -> Vec<Vec<u32>>
{
    let mut octmap: Vec<Vec<u32>> = Vec::new();
    
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

const OFFSETS: [(isize, isize); 4] = [(0, 1), (1, 0), (-1, 0), (0, -1)];

fn main()
{
    let arr = read_2d();
    let m = arr.len();
    let n = arr[0].len();
    
    let mut q: BTreeSet<(u32, usize, usize)> = BTreeSet::new();
    let mut costs: Vec<Vec<u32>> = vec![vec![u32::MAX; n]; m];
    costs[0][0] = 0;
    q.insert((0, 0, 0));

    while let Some((cost, i, j)) = q.pop_first() {
        if i == m - 1 && j == n - 1 { // Bottom right target
            println!("Min cost: {}", cost);
            return;
        }

        // Otherwise, time to insert neighbors
        for (ioff, joff) in OFFSETS.iter() {
            let ni = i as isize + ioff;
            let nj = j as isize + joff;
            if 0 <= ni && 0 <= nj  {
                let ni = ni as usize;
                let nj = nj as usize;
                if ni < n && nj < m {
                    let tentative_cost = cost + arr[ni][nj];
                    if tentative_cost < costs[ni][nj] {
                        if costs[ni][nj] < u32::MAX {
                            q.remove(&(costs[ni][nj], ni, nj));
                        } 
                        q.insert((tentative_cost, ni, nj));
                        costs[ni][nj] = tentative_cost;
                    }
                }
            }
        }
    }

}
