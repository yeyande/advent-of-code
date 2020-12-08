fn main() {
    let contents = include_str!("../input.txt");
    let code: Vec<&str> = contents.lines().into_iter().collect();
    let solution = execute(code);
    println!("{}", solution)
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Opcode {
    NOP,
    ACC,
    JMP
}

#[derive(Debug)]
struct Application {
    accumulator: i32,
    program: Vec<(Opcode, i32)>,
    executed: Vec<i32>,
    program_counter: i32,
    loop_detected: bool
}

impl Application {
    fn new(program: Vec<(Opcode, i32)>) -> Self {
        Application {
            accumulator: 0,
            program: program,
            executed: vec![],
            program_counter: 0,
            loop_detected: false,
        }
    }

    pub fn run(&mut self) {
        loop {
            match self.executed.contains(&self.program_counter) {
                true => self.loop_detected = true,
                false => self.executed.push(self.program_counter)
            };
            match self.loop_detected {
                true => return,
                false => {
                    let copy = self.program.clone();
                    let next_opcode = copy.get(self.program_counter as usize).clone();
                    match next_opcode {
                        Some((opcode, arg)) => self.step(&opcode, *arg),
                        None => return
                    }
                }
            }
        }
    }

    pub fn step(&mut self, opcode: &Opcode, arg: i32) {
        match opcode {
            Opcode::NOP => (),
            Opcode::JMP => self.program_counter = self.program_counter + arg - 1,
            Opcode::ACC => self.accumulator = self.accumulator + arg,
        }
        self.program_counter = self.program_counter + 1;
    }
}

fn parse_instruction(instruction: &str) -> Option<(Opcode, i32)> {
    let split: Vec<&str> = instruction.split(" ").collect();
    let opcode: Opcode = match split[0] {
        "nop" => Opcode::NOP,
        "jmp" => Opcode::JMP,
        "acc" => Opcode::ACC,
        _ => return None
    };
    let arg = split[1].parse().unwrap();
    Some((opcode, arg))
}

fn get_valid_program_result(operations: Vec<(Opcode, i32)>) -> Option<i32> {
    let mut application = Application::new(operations);
    application.run();
    match application.loop_detected {
        false => Some(application.accumulator),
        true => None,
    }
}

fn execute(code: Vec<&str>) -> i32 {
    let operations: Vec<(Opcode, i32)> = code.iter().filter_map(|x| parse_instruction(*x)).collect();
    let enumerated_repairs = enumerate_repairs(operations.clone());
    let valid_results: Vec<i32> = enumerated_repairs.into_iter().filter_map(get_valid_program_result).collect();
    valid_results[0]
}

fn enumerate_repairs(operations: Vec<(Opcode, i32)>) -> Vec<Vec<(Opcode, i32)>> {
    let mut repairs: Vec<Vec<(Opcode, i32)>> = vec![operations.clone()];
    let instructions_to_swap: Vec<usize> = operations.clone().into_iter().enumerate().filter(|(_, (op, _))| *op != Opcode::ACC).map(|(idx, _)| idx).collect();
    for idx in instructions_to_swap.iter() {
        let mut repaired = operations.clone();
        let (bad_op, arg) = repaired[*idx];
        let new_op = match bad_op {
            Opcode::NOP => Opcode::JMP,
            Opcode::JMP => Opcode::NOP,
            Opcode::ACC => Opcode::ACC,
        };
        repaired[*idx] = (new_op, arg);
        repairs.push(repaired.clone());
    }
    repairs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(
            execute(vec![
                "nop +0",
                "acc +1",
                "jmp +4",
                "acc +3",
                "jmp -3",
                "acc -99",
                "acc +1",
                "jmp -4",
                "acc +6",
            ]),
            8
        );
    }
}
