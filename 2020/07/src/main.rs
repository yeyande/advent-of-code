use std::collections::HashMap;

fn main() {
    println!("Hello, world!");
}

fn parse_rule(rule: &str) -> (&str, Vec<(usize, &str)>) {
    let key_bag, bag_connections = rule.split(" contain ")[0..1];
    let key: &str = key_bag.split(" bags")[0];
    let connections: Vec<(usize, &str)> = bag_connections.split(", ").map(|bag| 
            let count, bag_definition = bag.splitn(1, " ");
            let bag = bag_definition.replace("bag"
                                                 ).collect();
    return (key, connections)
}

fn parse_rules(rules: Vec<&str>) -> HashMap<&str, Vec<(usize, &str)>> {
    let graph: HashMap<&str, Vec<(usize, &str)>> = HashMap::new();
    rules.map(|rule| graph.insert(parse_rule)).collect();
    graph
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_rules_correctly() {
        let expected: HashMap<&str, Vec<(usize, &str)>> = vec![
                ("light red", vec![(1, "bright white"), (2, "muted yellow")]),
                ("dark orange", vec![(3, "bright white"), (4, "muted yellow")]),
                ("bright white", vec![(1, "shiny gold")]),
                ("muted yellow", vec![(2, "shiny gold"), (9, "faded blue")]),
                ("shiny gold", vec![(1, "dark olive"), (2, "vibrant plum")]),
                ("dark olive", vec![(3, "faded blue"), (4, "dotted black")]),
                ("vibrant plum", vec![(5, "faded blue"), (6, "dotted black")]),
                ("faded blue", vec![]),
                ("dotted black", vec![]),
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
