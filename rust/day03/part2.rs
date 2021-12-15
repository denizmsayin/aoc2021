use std::io;
use std::vec::Vec;
use std::cmp::Ordering;

fn binstr2boolvec(s: String) -> Option<Vec<bool>>
{
    let mut v: Vec<bool> = Vec::new();
    let binstr = s.trim();
    for ch in binstr.chars() {
        match ch {
            '0' => v.push(false),
            '1' => v.push(true),
            _ => { 
                println!("Unexpected character {} in binary string.", ch);
                return None;
            }
        };
    }
    return Some(v);
}

fn boolvec2u64(v: &Vec<bool>) -> u64
{
    let mut value = 0;
    let mut shift = 0;
    for bit_set in v.iter().rev() {
        if *bit_set {
            value += 1 << shift;
        }
        shift += 1;
    }
    return value;
}

fn first_true_ind(v: &Vec<bool>) -> Option<usize>
{
    for i in 0..v.len() {
        if v[i] {
            return Some(i);
        }
    }
    return None;
}

fn calculate_rating(diagnostics: &Vec<Vec<bool>>, most_common: bool) -> u64
{
    let binlen = diagnostics[0].len();

    // Kind of inefficient with a bool array, 
    // but I want to avoid using a set for some reason
    let mut to_check = vec![true; diagnostics.len()];
    let mut to_check_count = diagnostics.len();
    for i in 0..binlen {
        let mut ones_count = 0;
        for (j, binvec) in diagnostics.iter().enumerate() {
            if to_check[j] && binvec[i] {
                ones_count += 1;
            }
        }

        let zeros_count = to_check_count - ones_count;

        // Weird XOR with most_common here to switch logic
        let target = (ones_count < zeros_count) != most_common;

        for (j, binvec) in diagnostics.iter().enumerate() {
            if to_check[j] && binvec[i] != target {
                to_check[j] = false;
                to_check_count -= 1;
            }
        }

        if to_check_count == 1 {
            break;
        }
    }

    let i = first_true_ind(&to_check).expect("Logic error, no values left in set!");
    return boolvec2u64(&diagnostics[i]);
}

fn main()
{
    let mut diagnostics = Vec::new();

    loop {
        let mut line = String::new();
        
        let bytes = io::stdin()
            .read_line(&mut line)
            .expect("Unable to read line!");

        if bytes == 0 {
            break;
        }

        match binstr2boolvec(line) {
            Some(v) => diagnostics.push(v),
            None => return,
        }
    }

    let oxygen_gen_rating = calculate_rating(&diagnostics, true);
    let co2_scrubber_rating = calculate_rating(&diagnostics, false);
    println!("{}", oxygen_gen_rating * co2_scrubber_rating);

}
