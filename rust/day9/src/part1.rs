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
    let mut total_risk: u64 = 0;
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
                total_risk += 1 + x as u64;
            }
        }
    }

    println!("{}", total_risk);
}
