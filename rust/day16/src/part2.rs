use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn hexchar2value(c: char) -> u64
{
    let b = c as u64;
    if 'A' <= c && c <= 'F' {
        return b - b'A' as u64 + 10;
    } else if '0' <= c && c <= '9' {
        return b - b'0' as u64;
    }
    panic!("Not a hex char");
}

fn hexa2bits(s: &str) -> Vec<bool>
{
    let mut bits: Vec<bool> = Vec::new();
    for hex_char in s.chars() { // let's convert 32 bits at a time
        let x = hexchar2value(hex_char);
        for i in (0..4).rev() {
            bits.push(((x >> i) & 1) != 0);
        }
    }
    bits
}

fn bits2value(bs: &[bool]) -> u64
{
    let mut x = 0;
    for b in bs.iter() {
        x = (x << 1) + *b as u64;
    }
    x
}

fn parse_subpackets(packet: &[bool]) -> (Vec<u64>, usize)
{
    let len_type = bits2value(&packet[6..7]);
    let total_sub_len = bits2value(&packet[7..22]) as usize;
    let num_packets = bits2value(&packet[7..18]);
    let base_off = if len_type == 0 { 22 } else { 18 };
    let mut i = 0;
    let mut packets_so_far = 0;
    let mut values: Vec<u64> = Vec::new();
    while (len_type == 0 && i < total_sub_len) || (len_type == 1 && packets_so_far < num_packets)
    {
        let (value, sub_len) = parse_packet(&packet[i+base_off..]);
        i += sub_len;
        packets_so_far += 1;
        values.push(value);
    }
    (values, base_off + i)
}

fn parse_packet(packet: &[bool]) -> (u64, usize)
{
    let type_id = bits2value(&packet[3..6]);
    if type_id == 4 {
        // still have to parse to get length of packet
        let mut packet_length = 6;
        let mut value = 0;
        for slice in packet[6..].chunks_exact(5) {
            packet_length += 5;
            value = (value << 4) + bits2value(&slice[1..]);
            if !slice[0] {
                break;
            }
        }
        return (value, packet_length);
    } else {
        let (subvalues, len) = parse_subpackets(packet);
        let value: u64 = match type_id {
            0 => subvalues.iter().sum(),
            1 => subvalues.iter().product(),
            2 => *subvalues.iter().min().unwrap(),
            3 => *subvalues.iter().max().unwrap(),
            _ => { 
                assert! (subvalues.len() == 2);
                let a = subvalues[0];
                let b = subvalues[1];
                (match type_id {
                    5 => a > b,
                    6 => a < b,
                    7 => a == b,
                    _ => panic!("Bad type id"),
                }) as u64
            }
        };
        (value, len)
    }
}

fn main()
{
    let mut line = String::new();
    read_line_must(&mut line);
    let bits = hexa2bits(&line.trim());
    let (value, _) = parse_packet(&bits);
    println!("{}", value);
}
