#[macro_use]
extern crate lazy_static;
use regex::Regex;
use std::collections::HashMap;

fn main() {
    let contents = include_str!("../input.txt");
    let program: Vec<&str> = contents.lines().into_iter().collect();
    let solution = solve(program.clone());
    println!("{}", solution);
    let solution_2 = solve_2(program.clone());
    println!("{}", solution_2);
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

fn solve_2(program: Vec<&str>) -> u64 {
    let mut memory: HashMap<u64, u64> = HashMap::new();
    let mut mask: &str = "";
    for line in program.iter() {
        let split: Vec<&str>= line.split(" = ").collect();
        let (instruction, value): (&str, &str) = (split[0], split[1]);
        match instruction {
            "mask" => mask = value.clone(),
            _ => {
                lazy_static! {
                    static ref MEMORY_PARSER: Regex = Regex::new(r"mem\[(\d+)\]").unwrap();
                }
                match MEMORY_PARSER.captures(instruction) {
                    Some(address) => {
                        for addr in enumerate_addresses(&mask_address(&address[1], mask)).iter() {
                            memory.insert(*addr, value.parse().unwrap());
                        }
                    }
                    None => ()
                };
            }
        }
    }
    memory.values().sum()
}

fn enumerate_addresses(address: &str) -> Vec<u64> {
    match u64::from_str_radix(address, 2).ok() {
        Some(addr) => vec![addr],
        None => {
            let replaced = (address.replacen("X", "1", 1), address.replacen("X", "0", 1));
            let mut first = enumerate_addresses(&replaced.0);
            let second = enumerate_addresses(&replaced.1);
            first.extend(second);
            first
        }
    }
}

fn mask_address(address: &str, mask: &str) -> String {
    let parsed: String = format!("{:036b}", address.parse::<u64>().unwrap());
    mask.chars().zip(parsed.chars()).map(|(m, a)|
        match m {
            'X' => 'X',
            '1' => '1',
            _ => a
        }
    ).collect()
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
    fn enumerate_addresses_should_work() {
        let mask = "000000000000000000000000000000X1001X";
        assert_eq!(
            enumerate_addresses(mask),
            vec![0b110011, 0b110010, 0b10011, 0b10010]
        );
    }

    #[test]
    fn mask_address_should_work() {
        let mask = "000000000000000000000000000000X1001X";
        assert_eq!(
            mask_address("42", mask),
            "000000000000000000000000000000X1101X"
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

    #[test]
    fn solve_2_should_work() {
        assert_eq!(
            solve_2(vec![
                "mask = 000000000000000000000000000000X1001X",
                "mem[42] = 100",
                "mask = 00000000000000000000000000000000X0XX",
                "mem[26] = 1",
            ]),
            208
        );
    }
}
