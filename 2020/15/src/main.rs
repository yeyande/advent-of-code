use std::collections::HashMap;
fn main() {
    let contents = include_str!("../input.txt");
    let solution = get_nth_spoken_number(contents, 2020);
    println!("{}", solution);
}

fn get_nth_spoken_number(start: &str, n: usize) -> usize {
    let starting_numbers: Vec<usize> = start.trim().split(",").filter_map(|x| x.parse().ok()).collect();
    let mut database: HashMap<usize, usize> = HashMap::new();
    let mut last_spoken: usize = 0;
    for (turn, x) in starting_numbers.clone().into_iter().enumerate() {
        database.insert(x, turn+1);
        last_spoken = x;
    }
    let mut last_turn = starting_numbers.len();
    for turn in starting_numbers.len()+1..=n {
        last_spoken = match database.clone().get(&(last_spoken.clone())) {
            None => {
                database.insert(last_spoken.clone(), turn-1);
                0
            },
            Some(n) => {
                database.insert(last_spoken.clone(), turn-1);
                if turn == starting_numbers.len()+1 {
                    0
                } else {
                    last_turn-n
                }
            }
        };
        last_turn = turn;
    }
    last_spoken
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_nth_spoken_number_should_work() {
        assert_eq!(get_nth_spoken_number("0,3,6", 2020), 436);
        assert_eq!(get_nth_spoken_number("1,3,2", 2020), 1);
        assert_eq!(get_nth_spoken_number("2,1,3", 2020), 10);
        assert_eq!(get_nth_spoken_number("1,2,3", 2020), 27);
        assert_eq!(get_nth_spoken_number("2,3,1", 2020), 78);
        assert_eq!(get_nth_spoken_number("3,2,1", 2020), 438);
        assert_eq!(get_nth_spoken_number("3,1,2", 2020), 1836);

    }
}
