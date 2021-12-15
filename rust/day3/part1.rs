use std::io;
use std::cmp::Ordering;

fn main()
{
    // Could do with one, but this is simpler to read, I feel 
    let mut one_counts: [i32; 64] = [0; 64];
    let mut zero_counts: [i32; 64] = [0; 64];

    loop {
        let mut line = String::new();
        
        let bytes = io::stdin()
            .read_line(&mut line)
            .expect("Unable to read line!");

        if bytes == 0 {
            break;
        }

        let binstr = line.trim();

        for (i, ch) in binstr.chars().rev().enumerate() {
            match ch {
                '0' => zero_counts[i] += 1,
                '1' => one_counts[i] += 1,
                _ => { 
                    println!("Unexpected character {} in binary string.", ch);
                    return;
                }
            };
        }
    }

    let mut epsilon: u64 = 0;
    let mut gamma: u64 = 0;

    for i in 0..64 {
        if one_counts[i] > 0 || zero_counts[i] > 0 {
            match one_counts[i].cmp(&zero_counts[i]) {
                Ordering::Less => gamma += 1 << i,
                Ordering::Greater => epsilon += 1 << i,
                Ordering::Equal => {
                    println!("Equal bit counts at bit {}! Not sure what to do...", i);
                    return;
                }
            }
        }
    }

    println!("{}", epsilon * gamma);
}
