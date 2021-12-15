use std::io;

fn read_line_int() -> Option<i32> 
{
    let mut line = String::new();
    
    let bytes = io::stdin()
        .read_line(&mut line)
        .expect("Failed to read line!");
    
    if bytes == 0 {
        return None;
    }
    
    let value: i32 = line.trim().parse().expect("Expected an integer!");

    return Some(value);
}

fn main()
{
    let mut increases = 0;
    
    let mut value = match read_line_int() {
        Some(x) => x,
        None => { return; }
    };

    loop {
        let prev_value = value;

        value = match read_line_int() {
            Some(x) => x,
            None => { break; }
        };

        if value > prev_value {
            increases += 1;
        }
    }

    println!("Increases observed {} times", increases);
}
