use std::fs;

#[derive(Clone, Copy)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

#[derive(Clone, Copy)]
enum MatchResult {
    Win,
    Draw,
    Loss,
}

fn parse_input(contents: &str) -> Vec<(Choice, Choice)> {
    contents.split("\n")
        .map(|play| play.split(" ").filter_map(
                |c| match c {
                    "A" => Some(Choice::Rock),
                    "B" => Some(Choice::Paper),
                    "C" => Some(Choice::Scissors),
                    "X" => Some(Choice::Rock),
                    "Y" => Some(Choice::Paper),
                    "Z" => Some(Choice::Scissors),
                    _ =>   None,
                }
            )
            .collect::<Vec<Choice>>()
        ).filter_map(
          |play| 
            if play.len() > 0 { 
                Some((play[0], play[1])) 
            } else {
                    None
            }
        ).collect()
}

fn calculate_match_result(they: Choice, you: Choice) -> MatchResult {
    match they {
        Choice::Rock => {
            match you {
                Choice::Rock => MatchResult::Draw,
                Choice::Paper => MatchResult::Win,
                Choice::Scissors => MatchResult::Loss,
            }
        },
        Choice::Paper => {
            match you {
                Choice::Rock => MatchResult::Loss,
                Choice::Paper => MatchResult::Draw,
                Choice::Scissors => MatchResult::Win,
            }
        },
        Choice::Scissors => {
            match you {
                Choice::Rock => MatchResult::Win,
                Choice::Paper => MatchResult::Loss,
                Choice::Scissors => MatchResult::Draw,
            }
        },
    }
}

fn calculate_points(acc: u32, (play, result): (Choice, MatchResult)) -> u32 {
    let choice_point = match play {
        Choice::Rock => 1,
        Choice::Paper => 2,
        Choice::Scissors => 3,
    };

    let match_point = match result {
        MatchResult::Win => 6,
        MatchResult::Draw => 3,
        MatchResult::Loss => 0,
    };

    acc + choice_point + match_point
}

fn solve_1(contents: &str) -> u32 {
    let games = parse_input(contents);
    games.into_iter().map(|(they, you)| (you, calculate_match_result(they, you)))
         .fold(0, calculate_points)
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
            15
        )
    }
}
