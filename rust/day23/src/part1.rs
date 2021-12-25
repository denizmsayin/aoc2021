#![feature(map_first_last)]

use std::io;
use std::collections::HashMap;
use std::collections::BTreeSet;

// Decision: I'm going to do my worst here. Zero effort spend
// on encodings, using full states all the times as simple char
// arrays. Can't be that bad!

fn read_line_must(s: &mut String) -> usize {
    return io::stdin().read_line(s).expect("Unable to read line!");
}

const W: usize = 13;
const H: usize = 5;
const HALL_I: usize = 1;
const ROOMS_J: [usize; 4] = [3, 5, 7, 9];

type State = [[u8; W]; H];

fn read_state() -> State {
    let mut state: State = [[b' '; W]; H];
    for row in state.iter_mut() {
        let mut line = String::new();
        read_line_must(&mut line);
        for (i, &ch) in line.trim_end().as_bytes().iter().enumerate() {
            row[i] = ch;
        }
    }
    state
}

fn is_amph(c: u8) -> bool {
    b'A' <= c && c <= b'D'
}

fn in_room(state: &State, i: usize, j: usize) -> bool {
    i > 0 && i < W - 1 && state[i][j - 1] == b'#' && state[i][j + 1] == b'#'
}

fn get_final_room_j(c: u8) -> usize {
    match c {
        b'A' => ROOMS_J[0],
        b'B' => ROOMS_J[1],
        b'C' => ROOMS_J[2],
        b'D' => ROOMS_J[3],
        _ => panic!("Not an amph")
    }
}

fn room_is_final(state: &State, i: usize, j: usize) -> bool {
    in_room(state, i, j) && get_final_room_j(state[i][j]) == j
}

fn in_final_pos(state: &State, i: usize, j: usize) -> bool {
    if room_is_final(state, i, j) {
        let mut ii = i;
        while state[ii][j] == state[i][j] {
            ii += 1;
        }
        state[ii][j] == b'#'
    } else {
        false
    }
}

fn accessible_js(state: &State, i: usize, j: usize) -> Vec<usize> {
    let mut js = Vec::new();
    let mut i = i - 1;

    while state[i][j] == b'.' {
        i -= 1;
    }

    if state[i][j] == b'#' {
        i += 1;
        
        let mut jb = j - 1;
        while state[i][jb] == b'.' {
            if state[i+1][jb] == b'#' {
                js.push(jb);
            }
            jb -= 1;
        }

        let mut jf = j + 1;
        while state[i][jf] == b'.' {
            if state[i+1][jf] == b'#' {
                js.push(jf);
            }
            jf += 1;
        }
    }

    js
}

fn calc_cost(state: &State, i: usize, j: usize, i2: usize, j2: usize) -> isize {
    let dist = (i as isize - i2 as isize).abs() + (j as isize - j2 as isize).abs();
    let amph_cost = match state[i][j] {
        b'A' => 1,
        b'B' => 10,
        b'C' => 100,
        b'D' => 1000,
        _ => panic!("Not an amph")
    };
    dist * amph_cost
}

fn get_target_i_if_accessible(state: &State, j: usize) -> Option<usize> {
    let amph_type = state[HALL_I][j];
    let target_j = get_final_room_j(amph_type);
    let (s, e) = if j > target_j { (target_j, j) } else { (j + 1, target_j + 1) }; 

    for j in s..e {
        if state[HALL_I][j] != b'.' {
            return None;
        }
    }

    let mut i = HALL_I + 1;
    while state[i][target_j] == b'.' {
        i += 1;
    }

    let free_i = i - 1;

    while state[i][target_j] == amph_type {
        i += 1;
    }

    if state[i][target_j] == b'#' {
        Some(free_i)
    } else {
        None
    }
}

fn neighbors(state: &State) -> Vec<(isize, State)> {
    let mut ns = Vec::new();

    for i in 0..H {
        for j in 0..W {
            if is_amph(state[i][j]) {
                if i == HALL_I {
                    if let Some(target_i) = get_target_i_if_accessible(state, j) {
                        let target_j = get_final_room_j(state[i][j]);
                        let mut n = state.clone();
                        n[i][j] = b'.';
                        n[target_i][target_j] = state[i][j];
                        let cost = calc_cost(state, i, j, target_i, target_j);
                        ns.push((cost, n));
                    }
                } else {
                    if !in_final_pos(state, i, j) {
                        for k in accessible_js(state, i, j) {
                            let mut n = state.clone();
                            n[i][j] = b'.';
                            n[HALL_I][k] = state[i][j];
                            let cost = calc_cost(state, i, j, HALL_I, k);
                            ns.push((cost, n));
                        }
                    }
                }
            }
        }
    }

    ns
}

// fn print_state(s: &State) {
//     for row in s.iter() {
//         println!("{}", std::str::from_utf8(row).expect("encoding error!"));
//     }
// }

fn is_final_state(s: &State) -> bool {
    for i in 0..H {
        for j in 0..W {
            if is_amph(s[i][j]) && !in_final_pos(s, i, j) {
                return false;
            }
        }
    }
    true
}

fn main()
{
    let initial = read_state();

    let mut found = HashMap::new();
    let mut q = BTreeSet::new();
  
    found.insert(initial, 0isize);
    q.insert((0isize, initial));

    while let Some((cost, state)) = q.pop_first() {
//         print_state(&state);
        if is_final_state(&state) {
            println!("{}", cost);
            return;
        }
       
        for (neighbor_cost, neighbor) in neighbors(&state) {
            let tentative_cost = cost + neighbor_cost;
            let old_cost = found.entry(neighbor).or_insert(isize::MAX);
            
            if tentative_cost < *old_cost {
                if *old_cost != isize::MAX {
                    q.remove(&(*old_cost, neighbor));
                }
                q.insert((tentative_cost, neighbor));
                *old_cost = tentative_cost;
            }
        }
    }
}
