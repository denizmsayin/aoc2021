use std::io;

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_pos() -> usize {
    let mut s = String::new();
    read_line_must(&mut s);
    s.split(':').last().unwrap().trim().parse().unwrap()
}

fn add_tup(a: (u64, u64), b: (u64, u64)) -> (u64, u64) {
    (a.0 + b.0, a.1 + b.1)
}

fn mul_tup(x: u64, a: (u64, u64)) -> (u64, u64) {
    (x * a.0, x * a.1)
}

fn play(ppos: &mut [usize], pscore: &mut [usize], turn: usize, s: &mut [[[[[(u64, u64); 2]; 21]; 21]; 11]; 11]) -> (u64, u64) {
    if pscore[0] >= 21 {
        return (1, 0);
    } else if pscore[1] >= 21 {
        return (0, 1);
    }
    if s[ppos[0]][ppos[1]][pscore[0]][pscore[1]][turn] == (u64::MAX, u64::MAX) {
        let prev_pos = ppos[turn];
        let prev_score = pscore[turn];
        let mut r = (0, 0);
        // Combinations of three 1-2-3 dice rolls are fun:
        // 1: 3, 3: 4, 6: 5, 7: 6, 6: 7, 3: 8, 1: 9
        const TIMES: [u64; 10] = [0, 0, 0, 1, 3, 6, 7, 6, 3, 1];
        for d in 3..10 {
            ppos[turn] = (prev_pos + d - 1) % 10 + 1;
            pscore[turn] = prev_score + ppos[turn];
            r = add_tup(r, mul_tup(TIMES[d], play(ppos, pscore, (turn + 1) & 1, s)));
        }
        ppos[turn] = prev_pos;
        pscore[turn] = prev_score;
        s[ppos[0]][ppos[1]][pscore[0]][pscore[1]][turn] = r;
    }
    s[ppos[0]][ppos[1]][pscore[0]][pscore[1]][turn]
}

fn main()
{
    let mut states = [[[[[(u64::MAX, u64::MAX); 2]; 21]; 21]; 11]; 11];
    let mut ppos = [read_pos(), read_pos()];
    let mut pscore = [0, 0];

    let r = play(&mut ppos, &mut pscore, 0, &mut states);
    let winner_univs = std::cmp::max(r.0, r.1);
    
    println!("{}", winner_univs);
}
