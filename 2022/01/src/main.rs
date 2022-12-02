use std::fs;

fn solve_1(contents: &str) -> u32 {
    contents.split("\n\n")
        .map(|calories| 
            calories.split("\n")
                .map(|c| c.parse::<u32>())
                .filter(|r| r.is_ok())
                .fold(0, |acc, cal| acc + cal.unwrap())
                )
        .collect::<Vec<u32>>()
        .into_iter().max().unwrap_or(0)
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Could not read input");
    let solution_1 = solve_1(&contents);
    println!("{:?}", solution_1);
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
