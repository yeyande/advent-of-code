use std::fs;

fn parse_input(contents: &str) -> Vec<&str> {
    contents.split("\n")
        .collect()
}

#[derive(Debug, PartialEq)]
enum Color {
    Red,
    Green,
    Blue,
}

impl Color {
    fn from(color: &str) -> Option<Color> {
        match color {
          "red" => Some(Color::Red),
          "green" => Some(Color::Green),
          "blue" => Some(Color::Blue),
          _ => None
        }
    }
}

#[derive(Debug)]
struct Game {
    id: usize,
    pulls: Vec<(usize, Color)>,
}

fn parse_pull(pull: &str) -> Option<(usize, Color)> {
    match pull.trim().split_once(" ") {
        Some((count, color)) => {
            Some((count.parse::<usize>().unwrap(), Color::from(color).unwrap()))
        },
        None => None
    }
}

impl Game {
    fn from_string(record: &str) -> Option<Game> {
        match record.split_once(":") {
            Some((game, pulls)) => {
                let game_id = match game.split_once(" ") {
                    Some((_, x)) => x.parse::<usize>().unwrap(),
                    None => 0
                };
                let pulls = pulls.split(&[';', ','][..]).filter_map(parse_pull).collect();
                Some(Game {
                    id: game_id,
                    pulls: pulls,
                })
            }
            None => None
        }
    }
}

fn is_possible(pull: &(usize, Color)) -> bool {
    let (count, color) = pull;
    let maximums : Vec<(usize, Color)> = vec![
        (12, Color::Red),
        (13, Color::Green),
        (14, Color::Blue),
    ];
    !maximums.iter().filter(|(max_count, max_color)| *max_color == *color && max_count >= count ).collect::<Vec<&(usize, Color)>>().is_empty()
}

fn solve_1(contents: &str) -> u32 {
    let games: Vec<Game> = parse_input(contents).into_iter().filter_map(Game::from_string).collect();
    games
        .iter()
        .filter(|game| game.pulls.iter().all(is_possible))
        .fold(0, |acc, x| acc+(x.id as u32))
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
            8
        )
    }
}
