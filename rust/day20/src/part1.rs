use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn read_pixels(s: &str) -> Vec<bool> {
    let mut pixels = Vec::new();
    for c in s.trim().chars() {
        assert!(c == '.' || c == '#');
        pixels.push(c == '#');
    }
    pixels       
}

fn get_value(img: &Vec<Vec<bool>>, i: isize, j: isize, light: bool) -> bool {
    if i < 0 || j < 0 || i >= img.len() as isize || j >= img[0].len() as isize {
        light 
    } else {
        img[i as usize][j as usize]
    }
}

fn next_background(light: bool, algo: &Vec<bool>) -> bool {
    if light {
        algo[511]
    } else {
        algo[0]
    }
}

fn enhance(img: &Vec<Vec<bool>>, algo: &Vec<bool>, light: bool) -> Vec<Vec<bool>> {
    let mut enh: Vec<Vec<bool>> = Vec::new();    
    for i in -1..(img.len()+1) as isize {
        let mut pixline = Vec::new();
        for j in -1..(img[0].len()+1) as isize {
            let mut encoding = 0;
            for io in -1..2 {
                for jo in -1..2 {
                    let value = get_value(img, i + io, j + jo, light);
                    encoding <<= 1;
                    if value {
                        encoding |= 1;
                    }
                }
            }
            pixline.push(algo[encoding]);
        }
        enh.push(pixline);
    }
    enh
}

fn count_pixels(img: &Vec<Vec<bool>>) -> usize {
    img.concat().iter().filter(|x| **x).count()
}

fn main()
{
    let mut line = String::new();
    read_line_must(&mut line);
    let algo = read_pixels(&line);
    read_line_must(&mut String::new());
    let mut image: Vec<Vec<bool>> = Vec::new();
    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);
        if bytes == 0 {
            break;
        }
        let pixel_line = read_pixels(&line);
        image.push(pixel_line);
    }

    let mut light = false;
    let mut enhanced = image;
    for _ in 0..2 {
        enhanced = enhance(&enhanced, &algo, light);
        light = next_background(light, &algo);
    }
    
    println!("{}", count_pixels(&enhanced));
}
