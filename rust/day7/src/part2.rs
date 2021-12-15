use std::io;
use std::vec::Vec;

// Although the optimal solution is well known for the previous
// one, this one seems a bit non-trivial mathematically.
//
// However, the values seem to easily allow for brute-forcing. Good!

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_i64_vector() -> Vec<i64>
{
    let mut line = String::new();
    read_line_must(&mut line);
    return line.trim().split(',').map(|w| w.parse().unwrap()).collect();
}

fn main()
{
    let positions = read_i64_vector();

    let min = *positions.iter().min().unwrap();
    let max = *positions.iter().max().unwrap();

    let mut best_total: i64 = i64::MAX;
    for c in min..max+1 {
        let mut total = 0;
        for x in positions.iter() {
            let n = (x - c).abs();
            total += n * (n + 1) / 2;
        }
        if total < best_total {
            best_total = total;
        }
    }

    println!("Fuel used: {}", best_total);
}
