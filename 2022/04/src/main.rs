use std::fs;
use std::collections::HashSet;
use std::ops::Range;

fn parse_elf_assignment(elf: &str) -> HashSet<u32> {
    let mut range = elf.split("-");
    let start : u32 = range.next().unwrap().parse().unwrap();
    let end : u32 = range.next().unwrap().parse().unwrap();
    let mut assignments = HashSet::new();
    for i in (Range { start, end : end +1 }) {
        assignments.insert(i);
    }
    assignments
}

fn is_any_self_contained(assignments: &Vec<HashSet<u32>>) -> bool {
    let elf_1 = &assignments[0];
    let elf_2 = &assignments[1];
    elf_1.is_subset(&elf_2) || elf_2.is_subset(&elf_1)
}

fn solve_1(contents: &str) -> u32 {
    contents.split("\n")
            .filter_map(|assignments| if assignments.len() > 0 { Some(assignments.split(",").map(parse_elf_assignment).collect()) } else { None } ).collect::<Vec<Vec<HashSet<u32>>>>().into_iter()
            .filter(is_any_self_contained).collect::<Vec<Vec<HashSet<u32>>>>().len() as u32
}

fn solve_2(contents: &str) -> u32 {
    0
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
            2
        )
    }

    #[test]
    fn solve_2_works_on_sample_input() {
        let sample_input = fs::read_to_string("sample.txt").expect("Could not read sample input");
        assert_eq!(
            solve_2(&sample_input),
            0
        )
    }
}
