use std::io;
use std::vec::Vec;
use std::collections::HashSet;

const BOARD_SIZE: usize = 5;

type BingoBoard = [[u8; BOARD_SIZE]; BOARD_SIZE];

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_bingo_board() -> Option<BingoBoard>
{
    let mut b: BingoBoard = [[0; BOARD_SIZE]; BOARD_SIZE];

    read_line_must(&mut String::new()); // Throw away one line

    for i in 0..BOARD_SIZE {
        let mut line = String::new();
       
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            return None;
        }

        for (j, word) in line.trim().split_ascii_whitespace().enumerate() {
            b[i][j] = word.parse().expect("Expected an integer!");
        }
    }

    return Some(b);
}

fn read_u8_vector() -> Vec<u8>
{
    let mut line = String::new();
    read_line_must(&mut line);
    return line.trim().split(',').map(|w| w.parse().unwrap()).collect();
}

fn has_won(b: &BingoBoard, called_numbers: &HashSet<u8>) -> bool
{
    // Row check
    let mut win;
    for row in b {
        win = true;
        for value in row {
            if !called_numbers.contains(&value) {
                win = false;
                break;
            }
        }
        if win {
            return true;
        }
    }

    // Column check
    for j in 0..BOARD_SIZE {
        win = true;
        for i in 0..BOARD_SIZE {
            if !called_numbers.contains(&b[i][j]) {
                win = false;
                break;
            }
        }
        if win {
            return true;
        }
    }

    return false;
}

fn calculate_win_score(b: &BingoBoard, called_numbers: &HashSet<u8>, last_value: u8) -> u64
{
    // Assume the board has won
    let mut sum: u64 = 0;
    for row in b {
        for value in row {
            if !called_numbers.contains(&value) {
                sum += *value as u64;
            }
        }
    }

    return sum * last_value as u64;
}

fn main() 
{
    let values = read_u8_vector();
    let mut boards = Vec::new();

    loop {
        match read_bingo_board() {
            Some(b) => boards.push(b),
            None => break,
        }
    }

    let boards = boards;

    let mut called_numbers: HashSet<u8> = HashSet::new();
    for value in values {
        called_numbers.insert(value);

        for board in boards.iter() {
            if has_won(&board, &called_numbers) {
                let score = calculate_win_score(&board, &called_numbers, value);
                println!("{}", score);
                return;
            }
        }
    }

}
