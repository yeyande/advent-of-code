use std::collections::HashMap;

fn main() {
    let contents = include_str!("../input.txt");
    let rules: Vec<&str> = contents.lines().into_iter().collect();
    let solution_1: usize = solve_1(rules.clone(), "shiny gold".to_string());
    println!("{}", solution_1);
    let solution_2: usize = solve_2(rules.clone(), "shiny gold".to_string());
    println!("{}", solution_2);
}

fn get_nodes(connections: Vec<(String, String)>, key: String) -> Vec<String> {
    let mut not_visited: Vec<String> = connections.clone().iter().map(|(c, _)| c.to_string()).collect();
    not_visited.sort();
    not_visited.dedup();
    let mut connected: Vec<String> = connections.clone().iter().filter(|(_, conn)| conn.to_string() == key).map(|(bag, _)| bag.to_string()).collect();
    not_visited.retain(|e| ! connected.contains(e));
    for _ in 0..8 {
        for conn in connected.clone().iter() {
            let mut new_connections: Vec<String> = connections.clone().iter().filter(|(_, x)| x.to_string() == conn.to_string()).map(|(bag, _)| bag.to_string()).collect();
            connected.append(&mut new_connections);
        }
        not_visited.retain(|e| ! connected.contains(e));
    }
    connected.sort();
    connected.dedup();
    connected
}

fn get_all_connections(node: String, graph: HashMap<String, Vec<(usize, String)>>) -> Vec<String> {
    let connections: Vec<(String, String)> = graph.iter().map(|(k, v)| v.iter().map(|(_, bag)| (k.to_string(), bag.to_string())).collect::<Vec<(String, String)>>()).flatten().collect();
    let filtered: Vec<String> = get_nodes(connections.clone(), node);
    filtered
}

fn solve_1(rules: Vec<&str>, key: String) -> usize {
    let parsed = parse_rules(rules);
    //let x: Vec<String> = parsed.values().filter_map(|conn| get_node(conn.to_vec(), key.to_string())).collect();
    let x: Vec<String> = get_all_connections(key, parsed);
    x.len()
}

fn solve_2(rules: Vec<&str>, key: String) -> usize {
    let parsed = parse_rules(rules);
    let mut to_search: Vec<(usize, String)> = parsed.get(&key).unwrap().to_vec();
    let mut sum: usize = 0;
    loop {
        match to_search.pop() {
            None => break,
            Some((count, bag)) => {
                sum = sum + count;
                let mut new_searches: Vec<(usize, String)> = parsed.get(&bag).unwrap().to_vec().iter().map(|(c, b)| (c*count, b.to_string())).collect();
                to_search.append(&mut new_searches);
            }
        }
    }
    sum
}

fn parse_connection(bag: &str) -> Option<(usize, String)> {
    match bag {
        "no other bags." => None,
        _ => {
            let bag_info: Vec<&str> = bag.splitn(2, ' ').collect();
            let count = bag_info[0].parse().unwrap();
            let connection = bag_info[1].trim().replace(" bags", "").replace(" bag", "").replace(".", "");

            Some((count, connection))
        }
    }
}

fn parse_rule(rule: &str) -> (String, Vec<(usize, String)>) {
    let split: Vec<&str> = rule.split(" contain ").collect();
    let (key_bag, bag_connections) = (split[0], split[1]);
    let key: String = key_bag.replace(" bags", "");
    let connections: Vec<(usize, String)> = bag_connections
        .split(", ")
        .collect::<Vec<&str>>()
        .iter()
        .filter_map(|bag| parse_connection(*bag))
        .collect();
    return (key, connections)
}

fn parse_rules(rules: Vec<&str>) -> HashMap<String, Vec<(usize, String)>> {
    let mut graph: HashMap<String, Vec<(usize, String)>> = HashMap::new();
    for rule in rules.iter() {
        let (key, value) = parse_rule(rule);
        graph.insert(key, value);
    }
    graph
}

#[cfg(test)]
mod tests {
    use super::*;

    //#[test]
    //fn solve_1_should_work_for_sample_input() {
    //    assert_eq!(
    //        solve_1(vec![
    //            "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    //            "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    //            "bright white bags contain 1 shiny gold bag.",
    //            "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    //            "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    //            "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    //            "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    //            "faded blue bags contain no other bags.",
    //            "dotted black bags contain no other bags.",
    //        ], "shiny gold".to_string()),
    //        4
    //    );
    //}

    #[test]
    fn solve_2_should_work_for_sample_input() {
        assert_eq!(
            solve_2(vec![
                "light red bags contain 1 bright white bag, 2 muted yellow bags.",
                "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
                "bright white bags contain 1 shiny gold bag.",
                "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
                "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
                "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
                "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
                "faded blue bags contain no other bags.",
                "dotted black bags contain no other bags.",
            ], "shiny gold".to_string()),
            32
        );
    }

    #[test]
    fn should_parse_rules_correctly() {
        let expected: HashMap<String, Vec<(usize, String)>> = vec![
                ("light red".to_string(), vec![(1, "bright white".to_string()), (2, "muted yellow".to_string())]),
                ("dark orange".to_string(), vec![(3, "bright white".to_string()), (4, "muted yellow".to_string())]),
                ("bright white".to_string(), vec![(1, "shiny gold".to_string())]),
                ("muted yellow".to_string(), vec![(2, "shiny gold".to_string()), (9, "faded blue".to_string())]),
                ("shiny gold".to_string(), vec![(1, "dark olive".to_string()), (2, "vibrant plum".to_string())]),
                ("dark olive".to_string(), vec![(3, "faded blue".to_string()), (4, "dotted black".to_string())]),
                ("vibrant plum".to_string(), vec![(5, "faded blue".to_string()), (6, "dotted black".to_string())]),
                ("faded blue".to_string(), vec![]),
                ("dotted black".to_string(), vec![]),
            ].into_iter().collect();
        assert_eq!(
            parse_rules(vec![
                "light red bags contain 1 bright white bag, 2 muted yellow bags.",
                "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
                "bright white bags contain 1 shiny gold bag.",
                "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
                "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
                "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
                "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
                "faded blue bags contain no other bags.",
                "dotted black bags contain no other bags.",
            ]),
            expected
        );
    }
}
