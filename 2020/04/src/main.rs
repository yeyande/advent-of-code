use itertools::Itertools;
use std::fmt::Debug;
use std::collections::HashMap;

fn main() {
    let contents = include_str!("../input.txt");
    let batches: Vec<&str> = contents.lines().into_iter().collect();
    let solution: usize = get_valid_passports(batches).len();
    println!("{}", solution)
}

#[derive(Debug, Clone)]
struct Passport {
    byr: Option<String>,
    iyr: Option<String>,
    eyr: Option<String>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<String>
}

impl Passport {
    pub fn new(args: HashMap<&str, &str>) -> Passport {
        Passport {
            byr: args.get("byr").map(|&x| String::from(x)),
            iyr: args.get("iyr").map(|&x| String::from(x)),
            eyr: args.get("eyr").map(|&x| String::from(x)),
            hgt: args.get("hgt").map(|&x| String::from(x)),
            hcl: args.get("hcl").map(|&x| String::from(x)),
            ecl: args.get("ecl").map(|&x| String::from(x)),
            pid: args.get("pid").map(|&x| String::from(x)),
            cid: args.get("cid").map(|&x| String::from(x))
        }
    }
}


fn is_valid_passport(passport: Passport) -> bool {
    let required_fields = [passport.byr, passport.iyr, passport.eyr, passport.hgt, passport.hcl, passport.ecl, passport.pid];
    required_fields.iter().all(|x| *x != None)
}



fn parse_to_passport(group: Vec<&str>) -> Option<Passport> {
    let one_liner = group.join(" ");
    let parsed = one_liner.split(" ")
        .collect::<Vec<&str>>()
        .into_iter()
        .map(|x| x.split(":").collect::<Vec<&str>>())
        .collect::<Vec<Vec<&str>>>();
    let mut fields = HashMap::new();
    for v in parsed {
        fields.insert(v[0], v[1]);
    }
    let passport = Passport::new(fields);
    match is_valid_passport(passport.clone()) {
        true => Some(passport),
        false => None
    }
}

fn get_valid_passports(batch: Vec<&str>) -> Vec<Passport> {
    batch.into_iter()
        .group_by(|x| *x != "")
        .into_iter()
        .filter(|(matched, _)| *matched)
        .filter_map(|(_, g)| parse_to_passport(g.collect())).collect()
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
            ]).len(),
            2
        )
    }
}
