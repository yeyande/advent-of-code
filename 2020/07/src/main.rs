use std::collections::HashMap;

fn main() {
    let contents = include_str!("../input.txt");
    let rules: Vec<&str> = contents.lines().into_iter().collect();
    let solution: usize = solve(rules, "shiny gold".to_string());
    println!("{}", solution)
}

fn get_node(connections: Vec<(usize, String)>, key: String) -> Option<String> {
    let direct: Vec<String>= connections.iter().map(|(_, x)| x.clone()).filter(|x| *x == key).collect();
    match direct.len() {
        0 => None,
        _ => Some(key)
    }
}

fn get_all_connections(node: String, graph: HashMap<String, Vec<(usize, String)>>) -> Vec<String> {
    let mut connections: Vec<String> = vec![];
    graph.get(&node).unwrap().iter().map(|(_, conn)| get_connections_helper(connections.clone(), conn.to_string(), graph.clone()));
    println!("{:?}", connections);
    connections
}

fn get_connections_helper(mut connections: Vec<String>, connection: String, graph: HashMap<String, Vec<(usize, String)>>) {
    println!("{:?}", connections);
    connections.push(connection.clone());
    connections.append(&mut get_all_connections(connection.to_string(), graph.clone()))
}

fn solve(rules: Vec<&str>, key: String) -> usize {
    let parsed = parse_rules(rules);
    //let x: Vec<String> = parsed.values().filter_map(|conn| get_node(conn.to_vec(), key.to_string())).collect();
    let x: Vec<String> = get_all_connections(key, parsed);
    x.len()
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

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(
            solve(vec![
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
            4
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
