#[macro_use]
extern crate lazy_static;
use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;

fn main() {
    let contents = include_str!("../input.txt");
    let batches: Vec<&str> = contents.lines().into_iter().collect();
    let solution: usize = get_valid_passports(batches).len();
    println!("{}", solution)
}

fn is_valid_passport(passport: HashMap<String, String>) -> bool {
    [
        is_valid_birth_year(passport.get("byr").map(|x| x.clone())),
        is_valid_issue_year(passport.get("iyr").map(|x| x.clone())),
        is_valid_expiration_year(passport.get("eyr").map(|x| x.clone())),
        is_valid_height(passport.get("hgt").map(|x| x.clone())),
        is_valid_hair_color(passport.get("hcl").map(|x| x.clone())),
        is_valid_eye_color(passport.get("ecl").map(|x| x.clone())),
        is_valid_passport_id(passport.get("pid").map(|x| x.clone())),
    ]
    .iter()
    .all(|x| *x)
}

fn number_is_in_range(val: Option<String>, (min, max): (i32, i32)) -> bool {
    val.map(|x| x.parse::<i32>().unwrap()).map(|x| x >= min && x <= max).unwrap_or(false)
}

fn is_valid_birth_year(byr: Option<String>) -> bool {
    number_is_in_range(byr, (1920, 2002))
}

fn is_valid_issue_year(iyr: Option<String>) -> bool {
    number_is_in_range(iyr, (2010, 2020))
}

fn is_valid_expiration_year(eyr: Option<String>) -> bool {
    number_is_in_range(eyr, (2020, 2030))
}

fn is_valid_height(hgt: Option<String>) -> bool {
    lazy_static! {
        static ref FORMAT_PARSER: Regex = Regex::new(r"(\d+)(in|cm)").unwrap();
    }
    match hgt {
        Some(h) => match FORMAT_PARSER.captures(&h) {
            Some(c) => {
                let value: Option<String> = Some(String::from(&c[1]));
                let unit = &c[2];
                match unit {
                    "in" => number_is_in_range(value, (59, 76)),
                    "cm" => number_is_in_range(value, (150, 193)),
                    _ => false,
                }
            }
            None => false,
        },
        None => false,
    }
}

fn is_valid_hair_color(hcl: Option<String>) -> bool {
    lazy_static! {
        static ref FORMAT_PARSER: Regex = Regex::new(r"#[\da-f]{6}").unwrap();
    }
    match hcl {
        Some(c) => FORMAT_PARSER.is_match(&c),
        None => false,
    }
}

fn is_valid_eye_color(ecl: Option<String>) -> bool {
    lazy_static! {
        static ref FORMAT_PARSER: Regex = Regex::new(r"(amb|blu|brn|gry|grn|hzl|oth)").unwrap();
    }
    match ecl {
        Some(c) => FORMAT_PARSER.is_match(&c),
        None => false,
    }
}

fn is_valid_passport_id(pid: Option<String>) -> bool {
    lazy_static! {
        static ref FORMAT_PARSER: Regex = Regex::new(r"^[\d]{9}$").unwrap();
    }
    match pid {
        Some(id) => FORMAT_PARSER.is_match(&id),
        None => false,
    }
}

fn parse_to_passport(group: Vec<&str>) -> Option<HashMap<String, String>> {
    let one_liner = group.join(" ");
    let parsed = one_liner
        .split(' ')
        .collect::<Vec<&str>>()
        .into_iter()
        .map(|x| x.split(':').collect::<Vec<&str>>())
        .collect::<Vec<Vec<&str>>>();
    let mut fields : HashMap<String, String>= HashMap::new();
    for v in parsed {
        fields.insert(v[0].to_string(), v[1].to_string());
    }
    match is_valid_passport(fields.clone()) {
        true => Some(fields.clone()),
        false => None,
    }
}

fn get_valid_passports(batch: Vec<&str>) -> Vec<HashMap<String, String>> {
    batch
        .into_iter()
        .group_by(|x| *x != "")
        .into_iter()
        .filter(|(matched, _)| *matched)
        .filter_map(|(_, g)| parse_to_passport(g.collect()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(
            get_valid_passports(vec![
                "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
                "byr:1937 iyr:2017 cid:147 hgt:183cm",
                "",
                "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
                "hcl:#cfa07d byr:1929",
                "",
                "hcl:#ae17e1 iyr:2013",
                "eyr:2024",
                "ecl:brn pid:760753108 byr:1931",
                "hgt:179cm",
                "",
                "hcl:#cfa07d eyr:2025 pid:166559648",
                "iyr:2011 ecl:brn hgt:59in",
            ])
            .len(),
            2
        )
    }

    #[test]
    fn birth_year_validates_properly() {
        assert_eq!(is_valid_birth_year(Some(String::from("2002"))), true);
        assert_eq!(is_valid_birth_year(Some(String::from("1927"))), true);
        assert_eq!(is_valid_birth_year(Some(String::from("1969"))), true);
        assert_eq!(is_valid_birth_year(Some(String::from("2003"))), false);
        assert_eq!(is_valid_birth_year(Some(String::from("1919"))), false);
    }

    #[test]
    fn issue_year_validates_properly() {
        assert_eq!(is_valid_issue_year(Some(String::from("2015"))), true);
        assert_eq!(is_valid_issue_year(Some(String::from("2020"))), true);
        assert_eq!(is_valid_issue_year(Some(String::from("2003"))), false);
        assert_eq!(is_valid_issue_year(Some(String::from("2021"))), false);
    }

    #[test]
    fn expiration_year_validates_properly() {
        assert_eq!(is_valid_expiration_year(Some(String::from("2020"))), true);
        assert_eq!(is_valid_expiration_year(Some(String::from("2025"))), true);
        assert_eq!(is_valid_expiration_year(Some(String::from("2019"))), false);
        assert_eq!(is_valid_expiration_year(Some(String::from("2031"))), false);
    }

    #[test]
    fn height_validates_properly() {
        assert_eq!(is_valid_height(Some(String::from("60in"))), true);
        assert_eq!(is_valid_height(Some(String::from("76in"))), true);
        assert_eq!(is_valid_height(Some(String::from("190cm"))), true);
        assert_eq!(is_valid_height(Some(String::from("190in"))), false);
        assert_eq!(is_valid_height(Some(String::from("190"))), false);
    }

    #[test]
    fn hair_color_validates_properly() {
        assert_eq!(is_valid_hair_color(Some(String::from("#123abc"))), true);
        assert_eq!(is_valid_hair_color(Some(String::from("#e05ee3"))), true);
        assert_eq!(is_valid_hair_color(Some(String::from("#123abz"))), false);
        assert_eq!(is_valid_hair_color(Some(String::from("123abc"))), false);
    }

    #[test]
    fn eye_color_validates_properly() {
        assert_eq!(is_valid_eye_color(Some(String::from("amb"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("blu"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("brn"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("gry"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("grn"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("hzl"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("oth"))), true);
        assert_eq!(is_valid_eye_color(Some(String::from("wat"))), false);
    }

    #[test]
    fn passport_id_validates_properly() {
        assert_eq!(is_valid_passport_id(Some(String::from("000000001"))), true);
        assert_eq!(is_valid_passport_id(Some(String::from("976934668"))), true);
        assert_eq!(is_valid_passport_id(Some(String::from("1"))), false);
        assert_eq!(
            is_valid_passport_id(Some(String::from("0123456789"))),
            false
        );
    }
}
