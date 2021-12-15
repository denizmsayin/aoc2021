use std::io;

// The straightforward list-append simulation is woefully
// inefficient when the number of days increases. Instead,
// we can keep the number of fishes for each day left.

const DAYS: usize = 256;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn main()
{
    let mut fishies: [u64; 9] = [0; 9];
    
    let mut line = String::new();
    read_line_must(&mut line);
    for x in line.trim().split(',').map(|w| w.parse::<usize>().unwrap()) {
        fishies[x] += 1;
    }
    
    for _ in 0..DAYS {
        let zero_count = fishies[0];
        for i in 1..9 {
            fishies[i - 1] = fishies[i];
        }
        fishies[6] += zero_count;
        fishies[8] = zero_count;
    }

    let total_fishies: u64 = fishies.iter().sum();
    println!("{}", total_fishies);

}
