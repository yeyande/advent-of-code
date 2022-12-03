use std::fs;
use std::iter::Map;
use std::str::Split;

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

impl Into<u32> for Choice {
    fn into(self) -> u32 {
        match self {
            Choice::Rock => 1,
            Choice::Paper => 2,
            Choice::Scissors => 3,
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

impl From<MatchResult> for u32 {
    fn from(result: MatchResult) -> u32 {
        match result {
            MatchResult::Win => 6,
            MatchResult::Draw => 3,
            MatchResult::Loss => 0,
        }
    }
}

fn parse_input<F>(contents: &str, parser: F) -> Map<Split<&str>, F> 
    where F: Fn(&str) -> Vec<ParseResult> {
    contents.split("\n")
        .map(parser)
}

fn parse_part_1(play: &str) -> Vec<ParseResult> {
    play.split(" ").filter_map(
        |c| match c {
            "A" => Some(ParseResult::C(Choice::Rock)),
            "B" => Some(ParseResult::C(Choice::Paper)),
            "C" => Some(ParseResult::C(Choice::Scissors)),
            "X" => Some(ParseResult::C(Choice::Rock)),
            "Y" => Some(ParseResult::C(Choice::Paper)),
            "Z" => Some(ParseResult::C(Choice::Scissors)),
            _ =>   None,
        }
    )
    .collect()
}

fn parse_part_2(play: &str) -> Vec<ParseResult> {
    play.split(" ").filter_map(
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
    .collect()

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
            } },
    }
}

fn calculate_points(acc: u32, (play, result): (Choice, MatchResult)) -> u32 {
    let choice_point : u32 = play.into();
    let match_point : u32 = result.into();


    acc + choice_point + match_point
}

fn transform_part_1(play: Vec<ParseResult>) -> Option<(Choice, Choice)> {
    if play.len() > 0 { 
        Some((play[0].into(), play[1].into())) 
    } else {
        None
    }
}

fn transform_part_2(play: Vec<ParseResult>) -> Option<(Choice, Choice)> {
    if play.len() > 0 { 
        Some((play[0].into(), get_play_from_match_result(play[0].into(), play[1].into())))
    } else {
        None
    }
}

fn solve_1(contents: &str) -> u32 {
    solve(contents, parse_part_1, transform_part_1)
}

fn solve_2(contents: &str) -> u32 {
    solve(contents, parse_part_2, transform_part_2)
}

fn solve<F, T>(contents: &str, parser: F, transformer: T) -> u32 
    where F: Fn(&str) -> Vec<ParseResult>,
          T: Fn(Vec<ParseResult>) -> Option<(Choice, Choice)> {
    parse_input(contents, parser).filter_map(transformer)
        .map(|(they, you)| (you, calculate_match_result(they, you)))
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
