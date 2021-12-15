use std::io;
use std::vec::Vec;
use std::collections::HashMap;
use std::collections::HashSet;

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

fn get_paths_rec(graph: &HashMap<String, Vec<String>>, from: &str, to: &str, visited: &mut HashSet<String>, used_twice: bool) -> u64
{
    if from == to {
        return 1;
    }

    let mut paths = 0;
    for neighbor in graph[from].iter() {
        let is_small = is_small_cave(neighbor);
        let was_vis = visited.contains(neighbor);
        if !(neighbor == "start" || is_small && was_vis && used_twice) {
            if is_small && !was_vis {
                visited.insert(String::from(neighbor));
            }
            paths += get_paths_rec(graph, neighbor, to, visited, was_vis || used_twice);
            if is_small && !was_vis {
                visited.remove(neighbor);
            }
        }
    }
    paths
}

fn get_paths(graph: &HashMap<String, Vec<String>>, from: &str, to: &str) -> u64
{
    let mut visited: HashSet<String> = HashSet::new();
    visited.insert(String::from(from));
    return get_paths_rec(graph, from, to, &mut visited, false);
}

fn main()
{
    let graph = read_graph();
    let total = get_paths(&graph, "start", "end");
    println!("Paths: {}", total);
}
