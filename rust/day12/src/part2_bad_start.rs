use std::io;
use std::vec::Vec;
use std::collections::HashMap;

fn read_line_must(s: &mut String) -> usize 
{
    return io::stdin().read_line(s).expect("Unable to read line!");
}

fn parse_line(s: &str) -> (&str, &str)
{
    let v: Vec<&str> = s.trim().split('-').collect();
    assert!(v.len() == 2);
    (v[0], v[1])
}

fn ins_to_vec_map(map: &mut HashMap<String, Vec<String>>, k: &str, v: &str)
{
    let vstr = String::from(v);
    match map.get_mut(k) {
        Some(vec) => { 
            vec.push(vstr);
        },
        None => { 
            map.insert(String::from(k), vec![vstr]);
        }
    }
}

fn read_graph() -> HashMap<String, Vec<String>>
{
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();

    loop {
        let mut line = String::new();
        let bytes = read_line_must(&mut line);

        if bytes == 0 {
            break;
        }

        let (l, r) = parse_line(&line);
        ins_to_vec_map(&mut graph, l, r);
        ins_to_vec_map(&mut graph, r, l);
    }

    graph
}

fn is_small_cave(c: &str) -> bool
{
    c.chars().all(|c| c.is_lowercase())
}

fn counter_add(counter: &mut HashMap<String, usize>, key: &str)
{
    let count = counter.entry(String::from(key)).or_insert(0);
    *count += 1;
}

fn counter_get(counter: &HashMap<String, usize>, key: &str) -> usize
{
    *counter.get(key).unwrap_or(&0)
}

fn sc_visited_twice(counter: &HashMap<String, usize>) -> bool
{
    for v in counter.values() {
        if *v == 2 {
            return true;
        }
    }
    false
}

fn get_paths_rec(graph: &HashMap<String, Vec<String>>, from: &str, to: &str, visited: &mut HashMap<String, usize>) -> u64
{
    if from == to {
        return 1;
    }

    let mut paths = 0;
    let used_twice = sc_visited_twice(visited);
    for neighbor in graph[from].iter() {
        let is_special = neighbor == "start" || neighbor == "end";
        let is_small = is_small_cave(neighbor);
        let count = counter_get(visited, neighbor);
        if !(is_special && count == 1 || used_twice && is_small && count >= 1) {
            if is_small {
                counter_add(visited, neighbor);
            }
            paths += get_paths_rec(graph, neighbor, to, visited);
            if is_small {
                *visited.get_mut(neighbor).unwrap() -= 1;
            }
        }
    }

    paths
}

fn get_paths(graph: &HashMap<String, Vec<String>>, from: &str, to: &str) -> u64
{
    let mut visited: HashMap<String, usize> = HashMap::new();
    counter_add(&mut visited, "start");
    return get_paths_rec(graph, from, to, &mut visited);
}

fn main()
{
    let graph = read_graph();
    let total = get_paths(&graph, "start", "end");
    println!("Paths: {}", total);
}
