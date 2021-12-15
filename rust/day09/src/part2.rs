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

fn main()
{
    let mut heatmap: Vec<Vec<u8>> = Vec::new();
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            break;
        }

        heatmap.push(parse_line(&line));
    }
    
    let m = heatmap.len();
    let n = heatmap[0].len();
    let mut low_points: Vec<(usize, usize)> = Vec::new();

    for i in 0..m {
        for j in 0..n {
            let x = heatmap[i][j];
            let down = j + 1;
            let right = i + 1;

            let ltu = if j > 0 { x < heatmap[i][j-1] } else { true };
            let ltd = if down < n { x < heatmap[i][down] } else { true };
            let ltl = if i > 0 { x < heatmap[i-1][j] } else { true };
            let ltr = if right < m { x < heatmap[right][j] } else { true };

            if ltu && ltd && ltr && ltl {
                low_points.push((i, j));
            }
        }
    }

    let mut basin_sizes = Vec::new();
    for (li, lj) in low_points.iter() {
        let mut points = 0;
        let mut stack: Vec<(usize, usize)> = vec![(*li, *lj)];
        let mut marked: Vec<Vec<bool>> = vec![vec![false; n]; m];
        marked[*li][*lj] = true;

        while let Some((i, j)) = stack.pop() {
            let x = heatmap[i][j];

            if j > 0 {
                let y = heatmap[i][j-1];
                if y != 9 && x < y && !marked[i][j-1] {
                    marked[i][j-1] = true;
                    stack.push((i, j-1));
                }
            }
            
            if i > 0 {
                let y = heatmap[i-1][j];
                if y != 9 && x < y && !marked[i-1][j]{
                    marked[i-1][j] = true;
                    stack.push((i-1, j));
                }
            }
            
            if j < n - 1 {
                let y = heatmap[i][j+1];
                if y != 9 && x < y && !marked[i][j+1] {
                    marked[i][j+1] = true;
                    stack.push((i, j+1));
                }
            }
            
            if i < m - 1 {
                let y = heatmap[i+1][j];
                if y != 9 && x < y && !marked[i+1][j] {
                    marked[i+1][j] = true;
                    stack.push((i+1, j));
                }
            }
            points += 1;
        }

        basin_sizes.push(points);
    }

    basin_sizes.sort();
    basin_sizes.reverse();

    println!("{}", basin_sizes[0] * basin_sizes[1] * basin_sizes[2]);
}
