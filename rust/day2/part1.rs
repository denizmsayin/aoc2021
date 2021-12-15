use std::io;

// Many calls here... Wonder if there's a better way to parse.

fn trim_and_parse_must(s: String, p: &str) -> i64
{
    return s.trim().strip_prefix(p).unwrap().trim().parse().unwrap();
}

fn main()
{

    let mut depth: i64 = 0;
    let mut pos: i64 = 0;

    loop {
        // First time learning about ownership semantics in Rust!
        
        let mut line = String::new();

        let bytes = io::stdin()
            .read_line(&mut line)
            .expect("Unable to read line!");
        
        if bytes == 0 {
            break;
        }

        if line.starts_with("forward") {
            pos += trim_and_parse_must(line, "forward");
        } else if line.starts_with("up") {
            depth -= trim_and_parse_must(line, "up");
        } else if line.starts_with("down") {
            depth += trim_and_parse_must(line, "down");
        } else {
            println!("Unexpected command: {}", line);
            return;
        }

        if depth < 0 {
            depth = 0;
        }
    }

    println!("{}", depth * pos);
}
