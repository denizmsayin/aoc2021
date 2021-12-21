use std::io;

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_pos() -> u64 {
    let mut s = String::new();
    read_line_must(&mut s);
    s.split(':').last().unwrap().trim().parse().unwrap()
}

fn main()
{
    let mut ppos = [read_pos(), read_pos()];
    let mut pscore = [0, 0];
    let mut d = 1;
    
    let losing_score = 'a: loop {
        for i in 0..2 {
            ppos[i] = (ppos[i] + 3 * d + 3 - 1) % 10 + 1;
            d += 3;
            pscore[i] += ppos[i];
            if pscore[i] >= 1000 {
                break 'a pscore[(i + 1) % 2];
            }
        }
    };

    println!("{}", losing_score * (d - 1));
}
