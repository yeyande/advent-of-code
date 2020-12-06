use itertools::Itertools;

fn main() {
    let contents = include_str!("../input.txt");
    let batches: Vec<&str> = contents.lines().into_iter().collect();
    let solution: usize = get_answers(batches).iter().fold(0, |a, b| a + b);
    println!("{}", solution)
}

fn get_answers(answers: Vec<&str>) -> Vec<usize> {
    let groups : Vec<String> = answers
        .into_iter()
        .group_by(|x| *x != "")
        .into_iter()
        .filter(|(matched, _)| *matched)
        .map(|(_, mut x)| String::from(x.join("")))
        .collect();
    groups.iter().map(|x| {
        let mut chars: Vec<char> = x.chars().collect();
        chars.sort();
        chars.dedup();
        chars.len()
    }).collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(get_answers(vec![
                               "abc",
                               "",
                               "a",
                               "b",
                               "c",
                               "",
                               "ab",
                               "ac",
                               "",
                               "a",
                               "a",
                               "a",
                               "a",
                               "",
                               "b"]
                               ), [3, 3, 3, 1, 1]);
    }
}
