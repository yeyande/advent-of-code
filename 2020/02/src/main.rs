#[macro_use]
extern crate lazy_static;
use regex::Regex;

fn main() {
    let contents = include_str!("../input.txt");
    let passwords: Vec<&str> = contents.lines().into_iter().collect();
    println!("{}", get_valid_password_count(passwords));
}

fn parse_password(password: &str) -> Option<String> {
    lazy_static! {
        static ref FORMAT_PARSER: Regex = Regex::new(r"([\d]+)-([\d]+) (\w): (\w+)").unwrap();
    }
    let captures: Vec<String> = FORMAT_PARSER
        .captures(password)
        .unwrap()
        .iter()
        .map(|x| String::from(x.unwrap().as_str()))
        .collect();
    let pos_1: usize = captures[1].parse().unwrap();
    let pos_2: usize = captures[2].parse().unwrap();
    let (letter, password) = (captures[3].chars().next().unwrap(), &captures[4]);
    match password.chars().nth(pos_1 - 1) {
        Some(a) => match password.chars().nth(pos_2 - 1) {
            Some(b) => match (letter == a) ^ (letter == b) {
                true => Some(String::from(password)),
                false => None,
            },
            None => None,
        },
        None => None,
    }
}

fn get_valid_password_count(passwords: Vec<&str>) -> usize {
    let valid_passwords: Vec<String> = passwords.iter().filter_map(|x| parse_password(x)).collect();
    valid_passwords.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(
            get_valid_password_count(vec![
                "1-3 a: abcde",
                "1-3 b: cdefg",
                "2-9 c: ccccccccc",
                "1-6 c: xcccxcccccz"
            ]),
            1
        )
    }
}
