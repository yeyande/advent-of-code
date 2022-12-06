use std::fs;
use std::ops::Range;

fn parse_initial_state(state: &str) -> Vec<Vec<char>> {
    let mut input : Vec<&str> = state.split("\n").collect();
    let rows = input.pop().unwrap().split(" ").collect::<Vec<&str>>();
    let number_of_rows : usize = rows[rows.len()-2].parse().unwrap();
    let mut initial_state = vec![];
    for row in 0..number_of_rows {
        initial_state.push(vec![]);
    }
    for line in input {
        for row in (Range { start: 0, end: number_of_rows }) {
            let item = line.chars().nth(4*row+1).unwrap();
            if item != ' ' {
                initial_state[row].insert(0, item);
            }
        }
    }
    initial_state
}

struct Instruction {
    count: usize,
    from: usize,
    to: usize
}

fn parse_instruction(instruction: &str) -> Option<Instruction> {
    if instruction != "" {
        let split : Vec<&str> = instruction.split(" ").collect();
        Some(Instruction {
            count: split[1].parse().unwrap(),
            from: split[3].parse().unwrap(),
            to: split[5].parse().unwrap(),
        })
    } else {
        None
    }
}

fn solve_1(contents: &str) -> String {
    let mut input = contents.split("\n\n");
    let mut state = parse_initial_state(input.next().unwrap());
    let instructions = input.next().unwrap();
    for instruction in instructions.split("\n").filter_map(parse_instruction) {
        for _ in 0..instruction.count {
            match state[instruction.from-1].pop() {
                Some(item) => state[instruction.to-1].push(item),
                None => {}
            }
        }
    }
    state.into_iter().filter_map(|mut c| c.pop()).collect()
}

fn solve_2(contents: &str) -> String {
    "".to_string()
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
            "CMZ"
        )
    }

    #[test]
    fn solve_2_works_on_sample_input() {
        let sample_input = fs::read_to_string("sample.txt").expect("Could not read sample input");
        assert_eq!(
            solve_2(&sample_input),
            ""
        )
    }
}
