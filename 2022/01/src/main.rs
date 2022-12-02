use std::fs;

fn parse_input(contents: &str) -> Vec<u32> {
    contents.split("\n\n")
        .map(|calories| 
            calories.split("\n")
                .map(|c| c.parse::<u32>())
                .filter(|r| r.is_ok())
                .fold(0, |acc, cal| acc + cal.unwrap())
                )
        .collect()
}

fn solve_1(contents: &str) -> u32 {
    parse_input(contents).into_iter().max().unwrap_or(0)
}

fn solve_2(contents: &str) -> u32 {
    let mut calories = parse_input(contents);
    calories.sort();
    calories.reverse();
    calories[0] + calories[1] + calories[2]
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
            24000
        )
    }
}
