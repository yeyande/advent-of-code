use std::fs;

#[derive(Clone, Copy)]
enum ParseResult {
    C(Choice),
    R(MatchResult)
}

#[derive(Debug, Clone, Copy)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

impl From<ParseResult> for Choice {
    fn from(result: ParseResult) -> Self {
        match result {
            ParseResult::C(choice) => choice,
            _ => panic!("Could not convert result")
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum MatchResult {
    Win,
    Draw,
    Loss,
}

impl From<ParseResult> for MatchResult {
    fn from(result: ParseResult) -> Self {
        match result {
            ParseResult::R(result) => result,
            _ => panic!("Could not convert result")
        }
    }
}

fn parse_input_part_1(contents: &str) -> Vec<(Choice, Choice)> {
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

fn parse_input_part_2(contents: &str) -> Vec<(Choice, Choice)> {
    contents.split("\n")
        .map(|play| play.split(" ").filter_map(
                |c| match c {
                    "A" => Some(ParseResult::C(Choice::Rock)),
                    "B" => Some(ParseResult::C(Choice::Paper)),
                    "C" => Some(ParseResult::C(Choice::Scissors)),
                    "X" => Some(ParseResult::R(MatchResult::Loss)),
                    "Y" => Some(ParseResult::R(MatchResult::Draw)),
                    "Z" => Some(ParseResult::R(MatchResult::Win)),
                    _ =>   None,
                }
            )
            .collect::<Vec<ParseResult>>()
        ).filter_map(
          |play| 
            if play.len() > 0 { 
                Some((play[0].into(), get_play_from_match_result(play[0].into(), play[1].into())))
            } else {
                None
            }
        ).collect()
}

fn get_play_from_match_result(they: Choice, result: MatchResult) -> Choice {
    match result {
        MatchResult::Win => {
            match they {
                Choice::Rock => Choice::Paper,
                Choice::Paper => Choice::Scissors,
                Choice::Scissors => Choice::Rock,
            }
        },
        MatchResult::Draw => they,
        MatchResult::Loss => {
            match they {
                Choice::Rock => Choice::Scissors,
                Choice::Paper => Choice::Rock,
                Choice::Scissors => Choice::Paper,
            }
        }
    }
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
    let games = parse_input_part_1(contents);
    games.into_iter().map(|(they, you)| (you, calculate_match_result(they, you)))
         .fold(0, calculate_points)
}

fn solve_2(contents: &str) -> u32 {
    let games = parse_input_part_2(contents);
    games.into_iter().map(|(they, you)| (you, calculate_match_result(they, you)))
         .fold(0, calculate_points)
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

    #[test]
    fn solve_2_works_on_sample_input() {
        let sample_input = fs::read_to_string("sample.txt").expect("Could not read sample input");
        assert_eq!(
            solve_2(&sample_input),
            12
        )
    }
}
