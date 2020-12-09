use itertools::Itertools;

fn main() {
    let contents = include_str!("../input.txt");
    let cipher: Vec<&str> = contents.lines().into_iter().collect();
    let solution = solve(cipher, 25);
    println!("{}", solution)
}

fn sum(x: Vec<i64>) -> i64 {
    let s = x.iter().fold(0, |a, b| a+b);
    s
}

fn is_valid_number(value: i64, preamble: Vec<i64>) -> bool {
    let matches: Vec<Vec<i64>> = preamble.into_iter().combinations(2).filter(|x| sum(x.to_vec()) == value).collect();
    return matches.len() > 0
}

fn solve(cipher: Vec<&str>, preamble_size: usize) -> i64 {
    let parsed: Vec<i64> = cipher.iter().filter_map(|x| x.parse().ok()).collect();
    let (start, stop) : (usize, usize) = (preamble_size, parsed.len()-1);
    for n in start..stop {
        match is_valid_number(parsed[n].clone(), parsed[n-preamble_size..n].to_vec()) {
            true => continue,
            false => return parsed[n]
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(
            solve(vec![
                "35",
                "20",
                "15",
                "25",
                "47",
                "40",
                "62",
                "55",
                "65",
                "95",
                "102",
                "117",
                "150",
                "182",
                "127",
                "219",
                "299",
                "277",
                "309",
                "576",
            ], 5),
            127
        );
    }
}
