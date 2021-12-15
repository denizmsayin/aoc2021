use std::io;
use std::vec::Vec;

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
    let mut positions = read_i64_vector();

    positions.sort();

    let median = positions[positions.len() / 2];
    let mut total = 0;
    for x in positions {
        total += (x - median).abs();
    }

    println!("Fuel used: {}", total);
}
