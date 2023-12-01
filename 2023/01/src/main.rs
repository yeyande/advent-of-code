use std::fs;

fn parse_input(contents: &str) -> Vec<Vec<u32>> {
    contents.split("\n")
        .map(|s| 
            s.chars()
                .filter_map(|c| c.to_digit(10))
                .collect()
                )
        .collect()
}

fn solve_1(contents: &str) -> u32 {
    let parsed = parse_input(contents);
    parsed.into_iter().map(
        |k| 
        format!("{:?}{:?}", k.first().unwrap_or(&0), k.last().unwrap_or(&0)).parse::<u32>())
        .filter(|x| x.is_ok() )
        .map(|x| x.unwrap())
        .fold(0, |acc, x| acc+x)
}

fn solve_2(contents: &str) -> u32 {
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
        let sample_input = fs::read_to_string("sample.txt").expect("Could not read sample input");
        assert_eq!(
            solve_1(&sample_input),
            142
        )
    }
}
