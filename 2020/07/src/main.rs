use std::collections::HashMap;

fn main() {
    println!("Hello, world!");
}

fn parse_rule(rule: &str) -> (String, Vec<(usize, &str)>) {
    let split: Vec<&str> = rule.split(" contain ").collect();
    let (key_bag, bag_connections) = (split[0], split[1]);
    let key: String = key_bag.replace(" bags", "");
    let connections: Vec<(usize, &str)> = bag_connections.split(", ").collect::<Vec<&str>>().iter().map(|bag|
            let bag_info: Vec<&str> = bag.splitn(1, " ").collect(),
            let (count, connection): (usize, &str) = (bag_info[0].parse().unwrap(), bag_info[1]);

            //let bag = bag_definition.replace("bags", "").replace("bag", "").replace(".", "");
            (1, *connection)
             ).collect();
    return (key, connections)
}

fn parse_rules(rules: Vec<&str>) -> HashMap<String, Vec<(usize, &str)>> {
    let mut graph: HashMap<String, Vec<(usize, &str)>> = HashMap::new();
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
    fn should_parse_rules_correctly() {
        let expected: HashMap<String, Vec<(usize, &str)>> = vec![
                ("light red".to_string(), vec![(1, "bright white"), (2, "muted yellow")]),
                ("dark orange".to_string(), vec![(3, "bright white"), (4, "muted yellow")]),
                ("bright white".to_string(), vec![(1, "shiny gold")]),
                ("muted yellow".to_string(), vec![(2, "shiny gold"), (9, "faded blue")]),
                ("shiny gold".to_string(), vec![(1, "dark olive"), (2, "vibrant plum")]),
                ("dark olive".to_string(), vec![(3, "faded blue"), (4, "dotted black")]),
                ("vibrant plum".to_string(), vec![(5, "faded blue"), (6, "dotted black")]),
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
