#[macro_use]
extern crate lazy_static;
use regex::Regex;
use std::collections::HashMap;

fn main() {
    let contents = include_str!("../input.txt");
    let program: Vec<&str> = contents.lines().into_iter().collect();
    let solution = solve(program.clone());
    println!("{}", solution);
}

fn parse_mask(mask: &str) -> (u64, u64) {
    let and_mask = u64::from_str_radix(&mask.replace("X","1"), 2).unwrap();
    let or_mask = u64::from_str_radix(&mask.replace("X","0"), 2).unwrap();
    (or_mask, and_mask)
}

fn apply_mask(value: u64, (or_mask, and_mask): (u64, u64)) -> u64 {
    (value & and_mask) | or_mask
}

fn solve(program: Vec<&str>) -> u64 {
    let mut memory: HashMap<u64, u64> = HashMap::new();
    let mut mask: (u64, u64) = (0, 0);
    for line in program.iter() {
        let split: Vec<&str>= line.split(" = ").collect();
        let (instruction, value): (&str, &str) = (split[0], split[1]);
        match instruction {
            "mask" => mask = parse_mask(value),
            _ => {
                lazy_static! {
                    static ref MEMORY_PARSER: Regex = Regex::new(r"mem\[(\d+)\]").unwrap();
                }
                match MEMORY_PARSER.captures(instruction) {
                    Some(address) => memory.insert(address[1].parse().unwrap(), apply_mask(value.parse().unwrap(), mask)),
                    None => None
                };
            }
        }
    }
    memory.values().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_mask_should_work() {
        assert_eq!(
            parse_mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"),
                     (0b000000000000000000000000000001000000,
                      0b111111111111111111111111111111111101));
    }

    #[test]
    fn apply_mask_should_work() {
        let mask = (
            0b000000000000000000000000000001000000,
            0b111111111111111111111111111111111101);
        assert_eq!(
            apply_mask(11, mask),
            73
        );
        assert_eq!(
            apply_mask(101, mask),
            101
        );
        assert_eq!(
            apply_mask(0, mask),
            64
        );
    }

    #[test]
    fn solve_should_work() {
        assert_eq!(
            solve(vec![
                "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                "mem[8] = 11",
                "mem[7] = 101",
                "mem[8] = 0",
            ]),
            165
        );
    }
}
