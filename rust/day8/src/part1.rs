use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn main()
{
    let mut total = 0;
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            break;
        }

        let sides: Vec<&str> = line.trim().split('|').collect();
        assert!(sides.len() == 2);

        for word in sides[1].trim().split_ascii_whitespace() {
            let n = word.len();
            if 2 <= n && n <= 4 || n == 7 {
                total += 1;
            }
        }
    }

    println!("Total 1,4,7,8 appearances: {}", total);
}
