use std::fs; 
use std::collections::HashSet;

fn solve_1(contents: &str) -> usize {
    for idx in (0..contents.len()) {
        match contents.get(idx..idx+4) {
            Some(s) => {
                let mut unique: HashSet<char> = HashSet::new();
                for c in s.chars() {
                    unique.insert(c);
                }
                if unique.len() == s.len() {
                    return idx+4;
                }
            },
            None => {},
        }
    }
    0
}

fn solve_2(contents: &str) -> usize {
    for idx in (0..contents.len()) {
        match contents.get(idx..idx+14) {
            Some(s) => {
                let mut unique: HashSet<char> = HashSet::new();
                for c in s.chars() {
                    unique.insert(c);
                }
                if unique.len() == s.len() {
                    return idx+14;
                }
            },
            None => {},
        }
    }
    0
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
        let sample_input: Vec<(&str, usize)> = vec![
            ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
            ("nppdvjthqldpwncqszvftbrmjlhg", 6),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
        ];
        for (input, expected) in sample_input.iter() {
            assert_eq!(
                solve_1(input),
                (*expected)
            )
        }
    }

    #[test]
    fn solve_2_works_on_sample_input() {
        let sample_input: Vec<(&str, usize)> = vec![
            ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
            ("nppdvjthqldpwncqszvftbrmjlhg", 23),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
        ];
        for (input, expected) in sample_input.iter() {
            assert_eq!(
                solve_2(input),
                (*expected)
            )
        }
    }
}
