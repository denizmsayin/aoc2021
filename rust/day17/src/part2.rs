use std::io;
use std::vec::Vec;
use std::collections::HashSet;

// An overengineered, overperformant first try for the solution.
// Has some subtle mistakes: some divisors are missed due to an
// obviously bad algorithm... One more step is necessary after
// gathering prime divisors, but forget it!

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

fn generate_divisors(n: i64) -> Vec<i64>
{
    // Bad!
    let mut divs: Vec<i64> = vec![1];
    for d in 2..n+1 {
        if n % d == 0 {
            divs.push(d);
        }
    }
    divs
}

fn is_sum(n: i64) -> Option<i64>
{
    // Bad, but not too much!
    let mut sum = 0;
    let mut x = 0;
    while sum < n {
        x += 1;
        sum += x;
    }
    if n == sum { Some(x) } else { None }
}

fn generate_possible_vyn(y_target: i64) -> Vec<(i64, i64)>
{
    let mut yns: Vec<(i64, i64)> = Vec::new();
    let r = 2 * y_target;
    for n in generate_divisors(r.abs()) {
        let right = r / n;
        // Try the solution for n * (2*vy - n + 1) = 2 * k
        // Assuming right = 2*vy - n + 1
        let vy2 = right + n - 1;
        if vy2 % 2 == 0 {
            let vy = vy2 / 2;
            yns.push((vy, n));
        }
    }
    yns
}

fn possible_vxs(n: i64, x_min: i64, x_max: i64) -> Vec<i64>
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
        if let Some(vx) = is_sum(vxsum) {
            if vx < n {
                vxs.push(vx);
            }
        }
    }
    vxs
}


fn main()
{
    let ((xs, xe), (ys, ye)) = read_target_area();
    let mut velocities: HashSet<(i64, i64)> = HashSet::new();

    for y in ys..ye+1 {
        let candidate_vyn = generate_possible_vyn(y);
        for (vy, n) in candidate_vyn.iter() {
            let n = *n;
            let vy = *vy;
            for vx in possible_vxs(n, xs, xe) {
                velocities.insert((vx, vy));
            }
        }
    }

    println!("{}", velocities.len());
}
