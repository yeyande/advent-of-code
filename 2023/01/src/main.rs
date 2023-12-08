use std::fs;

fn parse_input(contents: &str) -> Vec<&str> {
    contents.split("\n")
        .collect()
}

fn solve_1(contents: &str) -> u32 {
    let parsed = parse_input(contents);
    parsed.into_iter()
        .map(|l| 
            l.chars()
                .filter_map(|c| c.to_digit(10))
                .collect::<Vec<u32>>()
                )
        .filter_map(
        |k| 
        format!("{:?}{:?}", k.first().unwrap_or(&0), k.last().unwrap_or(&0)).parse::<u32>().ok())
        .fold(0, |acc, x| acc+x)
}

fn word_to_number(e: &str) -> Option<u32> {
    match e {
        "one" | "1" => Some(1),
        "two" | "2" => Some(2),
        "three" | "3" => Some(3),
        "four" | "4" => Some(4),
        "five" | "5" => Some(5),
        "six" | "6" => Some(6),
        "seven" | "7" => Some(7),
        "eight" | "8" => Some(8),
        "nine" | "9" => Some(9),
        _ => None
    }
}

fn solve_2(contents: &str) -> u32 {
    let parsed = parse_input(contents);
    let worded_number_mapping = [
        (1, "1"), 
        (2,"2"), 
        (3,"3"), 
        (4,"4"), 
        (5,"5"), 
        (6,"6"), 
        (7,"7"), 
        (8,"8"), 
        (9,"9"),
        (1, "one"), 
        (2,"two"), 
        (3,"three"), 
        (4,"four"), 
        (5,"five"), 
        (6,"six"), 
        (7,"seven"), 
        (8,"eight"), 
        (9,"nine"),
    ];
    parsed.into_iter()
        .filter_map(|l| {
                let number_positions: Vec<(usize, u32)>= worded_number_mapping.into_iter().filter_map(|n|
                    match l.match_indices(n.1).collect::<Vec<(usize, &str)>>().as_slice() {
                        [] => None,
                        x => {
                            Some(x.into_iter().map(|(p, e)| (*p, word_to_number(e).unwrap_or(0))).collect::<Vec<(usize, u32)>>())
                        }
                    }
                ).collect::<Vec<Vec<(usize, u32)>>>().into_iter().flatten().collect();
                let first = number_positions.clone().into_iter().min_by_key(|x| x.0).unwrap_or((0, 0)).1;
                let last = number_positions.clone().into_iter().max_by_key(|x| x.0).unwrap_or((0, 0)).1;
                format!("{:?}{:?}", first, last).parse::<u32>().ok()
            }
        )
        .fold(0, |acc, x| acc+x)
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Could not read input");
    let solution_1 = solve_1(&contents);
    println!("{:?}", solution_1);
    let solution_2 = solve_2(&contents);
    println!("{:?}", solution_2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_1_works_on_sample_input() {
        let sample_input = fs::read_to_string("sample_01.txt").expect("Could not read sample input");
        assert_eq!(
            solve_1(&sample_input),
            142
        )
    }

    #[test]
    fn solve_2_works_on_sample_input() {
        let sample_input = fs::read_to_string("sample_02.txt").expect("Could not read sample input");
        assert_eq!(
            solve_2(&sample_input),
            281
        )
    }
}
