use std::io;
use std::vec::Vec;

const DAYS: u64 = 256;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_u8_vector() -> Vec<u8>
{
    let mut line = String::new();
    read_line_must(&mut line);
    return line.trim().split(',').map(|w| w.parse().unwrap()).collect();
}

fn main()
{
    let mut fishies = read_u8_vector();
    
    for day in 1..DAYS+1 {
        for i in 0..fishies.len() {
            if fishies[i] == 0 {
                fishies[i] = 6;
                fishies.push(8);
            } else {
                fishies[i] -= 1;
            }
        }
        println!("Fishes after day {}: {}", day, fishies.len());
    }

}
