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

fn read_line_int_must() -> i32
{
    return read_line_int().expect("Must get an integer!");
}

fn main()
{
    let mut increases = 0;

    let mut first = read_line_int_must();
    let mut second = read_line_int_must();
    let mut third = read_line_int_must();
    let mut sum = first + second + third;

    loop {
        let next = match read_line_int() {
            Some(x) => x,
            None => { break; }
        };

        first = second;
        second = third;
        third = next;

        let next_sum = first + second + third;

        if next_sum > sum {
            increases += 1;
        }

        sum = next_sum;
    }
    
    println!("Increases observed {} times", increases);
}
