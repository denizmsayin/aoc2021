use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_grid() -> Vec<Vec<u8>> {
    let mut g: Vec<Vec<u8>> = Vec::new();
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
    
        if bytes == 0 {
            break;
        }

        g.push(Vec::from(line.trim().as_bytes()));
    }
    g
}

fn step_grid(g: &mut Vec<Vec<u8>>) -> bool {
    let m = g.len();
    let n = g[0].len();
    let mut changed = false;

    // Mark for move
    for i in 0..m {
        for j in 0..n {
            let nj = (j + 1) % n;
            if g[i][j] == b'>' && g[i][nj] == b'.' {
                g[i][j] = b'*';
                changed = true;
            }
        }
    }
    
    // Move
    for i in 0..m {
        for j in 0..n {
            if g[i][j] == b'*' {
                let nj = (j + 1) % n;
                g[i][nj] = b'>';
                g[i][j] = b'.';
            }
        }
    }

    for i in 0..m {
        for j in 0..n {
            let ni = (i + 1) % m;
            if g[i][j] == b'v' && g[ni][j] == b'.' {
                g[i][j] = b'*';
                changed = true;
            }
        }
    }
    
    // Move
    for i in 0..m {
        for j in 0..n {
            if g[i][j] == b'*' {
                let ni = (i + 1) % m;
                g[ni][j] = b'v';
                g[i][j] = b'.';
            }
        }
    }

    changed
}

// fn print_grid(g: &Vec<Vec<u8>>) {
//     for row in g.iter() {
//         println!("{}", std::str::from_utf8(row).unwrap());
//     }
// }

fn main()
{
    let mut g = read_grid();
    let mut i = 1;

    while step_grid(&mut g) {
        i += 1;
    }

    println!("{}", i);
}
