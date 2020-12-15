use std::collections::HashMap;
fn main() {
    let contents = include_str!("../input.txt");
    let solution = get_nth_spoken_number(contents, 30000000);
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
        let previous_last_spoken = last_spoken.clone();
        last_spoken = match database.get(&(last_spoken.clone())) {
            None => {
                0
            },
            Some(n) => {
                if turn == starting_numbers.len()+1 {
                    0
                } else {
                    last_turn-n
                }
            }
        };
        database.insert(previous_last_spoken, turn-1);
        last_turn = turn;
    }
    last_spoken
}


#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! input_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let ((start, nth), expected) = $value;
                assert_eq!(
                    expected,
                    get_nth_spoken_number(start, nth)
                );
            }
        )*
        }
    }

    input_tests! {
        test_1: (("0,3,6", 2020), 436),
        test_2: (("1,3,2", 2020), 1),
        test_3: (("2,1,3", 2020), 10),
        test_4: (("1,2,3", 2020), 27),
        test_5: (("2,3,1", 2020), 78),
        test_6: (("3,2,1", 2020), 438),
        test_7: (("3,1,2", 2020), 1836),

        test_08: (("0,3,6", 30000000), 175594),
        test_09: (("1,3,2", 30000000), 2578),
        test_10: (("2,1,3", 30000000), 3544142),
        test_11: (("1,2,3", 30000000), 261214),
        test_12: (("2,3,1", 30000000), 6895259),
        test_13: (("3,2,1", 30000000), 18),
        test_14: (("3,1,2", 30000000), 362),
    }
}
