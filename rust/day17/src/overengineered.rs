use std::io;
use std::vec::Vec;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_range(s: &str) -> (i64, i64)
{
    let vec: Vec<&str> = s[2..].split("..").collect();
    assert!(vec.len() == 2);
    (vec[0].parse().unwrap(), vec[1].parse().unwrap())
}

fn read_target_area() -> ((i64, i64), (i64, i64))
{
    let mut line = String::new();
    read_line_must(&mut line);
    let vec: Vec<&str> = line.trim().strip_prefix("target area: ").unwrap().split(", ").collect();
    assert!(vec.len() == 2);
    let xr = parse_range(vec[0]);
    let yr = parse_range(vec[1]);
    (xr, yr)
}

fn generate_primes(n: i64) -> Vec<i64>
{
    // Sieve!
    let n = n as usize;
    let mut sieve: Vec<bool> = vec![false; n];
    let mut primes: Vec<i64> = Vec::new();
    for x in 2..n {
        if !sieve[x] {
            primes.push(x as i64);
            for y in (x..n).step_by(x) {
                sieve[y] = true;
            }
        }
    }
    primes   
}

fn generate_sums(n: i64) -> Vec<i64>
{
    let mut sums: Vec<i64> = Vec::new();
    let mut sum = 0;
    let mut x = 1;
    while sum < n {
        sums.push(sum);
        sum += x;
        x += 1;
    }
    sums
}

fn generate_possible_vyn(primes: &[i64], y_target: i64) -> Vec<(i64, i64)>
{
    let mut yns: Vec<(i64, i64)> = Vec::new();
    let mut left = 2 * y_target.abs();
    let mut right = if y_target >= 0 { 1 } else { -1 };
    loop {
        // Try the solution for n * (2*vy - n + 1) = 2 * k
        // Assuming left = n, right = 2*vy - n + 1
        let vy2 = right + left - 1;
        if vy2 % 2 == 0 {
            let vy = vy2 / 2;
            yns.push((vy, left));
        }

        if left == 1 {
            break;
        }

        // Update left/right
        for p in primes.iter() {
            assert!(p != primes.last().unwrap());
            if left % p == 0 {
                left /= p;
                right *= p;
                break;
            }
        }
    }
    yns
}

fn possible_vxs(n: i64, x_min: i64, x_max: i64, sum_nums: &[i64]) -> Vec<i64>
{
    let mut vxs = Vec::new();
    // Inequality for vx, assuming vx > n
    // sx + n*(n-1)/2 <= n*vx <= ex + n*(n-1)/2
    // Just test values in the range for % n == 0.
    let nsum = n * (n - 1) / 2;
    for nvx in (x_min + nsum)..(x_max + nsum + 1) {
        if nvx % n == 0 {
            let vx = nvx / n;
            if vx >= n {
                vxs.push(vx);
            }
        }
    }
    // What about vx < n? That's more of an issue...
    // sx <= vx * (vx + 1) / 2 <= 2ex 
    // A check using sqrt is quite possible, but I want
    // to avoid doubles for a cleaner program. I guess
    // a slice to binary search through is the best...
    // Directly calculating the sum on the fly here also
    // would work, sqrt(n) instead of log(n), but no
    // preprocessing.
    for vxsum in x_min..x_max + 1 {
        assert!(vxsum <= *sum_nums.last().unwrap());
        if let Ok(i) = sum_nums.binary_search(&vxsum) {
            let vx = i as i64;
            if vx < n {
                vxs.push(vx);
            }
        }
    }
    vxs
}

const N_LIMIT: i64 = 100000;

fn main()
{
    let ((xs, xe), (ys, ye)) = read_target_area();
    
    let primes = generate_primes(N_LIMIT);
    let sums = generate_sums(N_LIMIT);

    let mut possible_count = 0;
    for y in ys..ye+1 {
        let candidate_vyn = generate_possible_vyn(&primes, y);
        for (vy, n) in candidate_vyn.iter() {
            let n = *n;
            let vy = *vy;
            for vx in possible_vxs(n, xs, xe, &sums) {
                println!("{},{}", vx, vy);
                possible_count += 1;
            }
        }
    }

    println!("{}", possible_count);
}
