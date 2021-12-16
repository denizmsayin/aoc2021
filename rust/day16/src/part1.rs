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

fn parse_packet(packet: &[bool]) -> (u64, usize)
{
    let mut vn_sum = bits2value(&packet[0..3]);
    let type_id = bits2value(&packet[3..6]);
    if type_id == 4 {
        // still have to parse to get length of packet
        let mut packet_length = 6;
        for slice in packet[6..].chunks_exact(5) {
            packet_length += 5;
            if !slice[0] {
                break;
            }
        }
        return (vn_sum, packet_length);
    } else {
        let len_type = bits2value(&packet[6..7]);
        if len_type == 0 {
            let total_sub_len = bits2value(&packet[7..22]) as usize;
            let mut i: usize = 0;
            while i < total_sub_len {
                let (vn, sub_len) = parse_packet(&packet[i+22..]);
                vn_sum += vn;
                i += sub_len;
            }
            return (vn_sum, 22 + total_sub_len);
        } else if len_type == 1 {
            let num_packets = bits2value(&packet[7..18]);
            let mut i: usize = 0;
            let mut packets_so_far = 0;
            while packets_so_far < num_packets {
                let (vn, sub_len) = parse_packet(&packet[i+18..]);
                vn_sum += vn;
                i += sub_len;
                packets_so_far += 1;
            }
            return (vn_sum, 18 + i);
        } else {
            panic!("bad parsing");
        }
    }
}

fn main()
{
    let mut line = String::new();
    read_line_must(&mut line);
    let bits = hexa2bits(&line.trim());
    let (vsum, _) = parse_packet(&bits);
    println!("{}", vsum);
}
