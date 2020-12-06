use itertools::Itertools;

fn main() {
    let contents = include_str!("../input.txt");
    let batches: Vec<&str> = contents.lines().into_iter().collect();
    let solution: usize = get_answers(batches).iter().sum();
    println!("{}", solution)
}

fn get_answers(answers: Vec<&str>) -> Vec<usize> {
    let answer_values: Vec<char> = "abcdefghijklmnopqrstuvwxyz".chars().collect();
    let groups: Vec<Vec<String>> = answers
        .into_iter()
        .group_by(|x| *x != "")
        .into_iter()
        .filter(|(matched, _)| *matched)
        .map(|(_, x)| x.map(String::from).collect())
        .collect();
    groups
        .iter()
        .map(|x| {
            let respondents = x.len();
            answer_values
                .clone()
                .into_iter()
                .filter_map(|c| {
                    let matches: Vec<usize> = x.iter().filter_map(|a| a.find(c)).collect();
                    match matches.len() == respondents {
                        true => Some(1),
                        false => None,
                    }
                })
                .sum()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(
            get_answers(vec![
                "abc", "", "a", "b", "c", "", "ab", "ac", "", "a", "a", "a", "a", "", "b"
            ]),
            [3, 0, 1, 1, 1]
        );
    }
}
