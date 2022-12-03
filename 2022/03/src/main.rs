use std::fs;
use std::slice::Chunks;

fn item_to_priority(c: char) -> u8 {
    let start_of_lower_case: u8 = 97;
    let start_of_upper_case: u8 = 65;
    if c.is_ascii_lowercase() {
        (c as u8) - start_of_lower_case + 1
    } else {
        (c as u8) - start_of_upper_case + 27
    }
}

fn get_common_bag_items(mut compartments: Chunks<char>) -> Vec<char> {
    let compartment_1 = compartments.next().unwrap();
    let compartment_2 = compartments.next().unwrap();
    let mut common_values: Vec<char> = compartment_1.iter().filter(|c| compartment_2.contains(c)).map(|c| *c).collect();
    common_values.sort();
    common_values.dedup();
    common_values
}

fn solve_1(contents: &str) -> u32 {
    contents.split("\n")
        .filter_map(|bag| 
            if bag.len() > 0 { 
                Some(get_common_bag_items(
                        bag.chars()
                            .collect::<Vec<char>>()
                            .chunks(bag.len()/2)).into_iter()
                    .map(item_to_priority)
                    .fold(0, |acc, priority| acc + (priority as u32))
                ) 
            } 
            else { 
                None 
            }
        )
        .fold(0, |acc, priority| acc + (priority as u32))
}

fn get_common_items_between_bags(bags: &[&str]) -> Vec<char> {
    let bag_1 = bags[0];
    let bag_2 = bags[1];
    let bag_3 = bags[2];
    let mut common_values: Vec<char> = bag_1.chars()
        .filter(|c| bag_2.contains(*c) && bag_3.contains(*c))
        .collect();
    common_values.sort();
    common_values.dedup();
    common_values
}

fn solve_2(contents: &str) -> u32 {
    contents.split("\n")
        .collect::<Vec<&str>>()
        .as_slice()
        .chunks(3)
        .filter_map(|bags| 
            if bags.len() > 1 { 
                Some(
                    get_common_items_between_bags(bags).into_iter()
                        .map(item_to_priority)
                        .fold(0, |acc, priority| acc + (priority as u32))
                ) 
            } 
            else { 
                None 
            }
        )
        .fold(0, |acc, priority| acc + (priority as u32))
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
            157
        )
    }

    #[test]
    fn solve_2_works_on_sample_input() {
        let sample_input = fs::read_to_string("sample.txt").expect("Could not read sample input");
        assert_eq!(
            solve_2(&sample_input),
            70
        )
    }
}
