fn main() {
    let contents = include_str!("../input.txt");
    let code: Vec<&str> = contents.lines().into_iter().collect();
    let solution = execute(code);
    println!("{}", solution)
}

#[derive(Debug, Clone, Copy)]
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
    halted: bool
}

impl Application {
    fn new(program: Vec<(Opcode, i32)>) -> Self {
        Application {
            accumulator: 0,
            program: program,
            executed: vec![],
            program_counter: 0,
            halted: false,
        }
    }

    pub fn run(&mut self) {
        loop {
            match self.executed.contains(&self.program_counter) {
                true => self.halted = true,
                false => self.executed.push(self.program_counter)
            };
            match self.halted {
                true => return,
                false => {
                    let (opcode, arg) = self.program[self.program_counter as usize];
                    self.step(&opcode, arg)
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

fn execute(code: Vec<&str>) -> i32 {
    let operations: Vec<(Opcode, i32)> = code.iter().filter_map(|x| parse_instruction(*x)).collect();
    let mut application = Application::new(operations);
    application.run();
    application.accumulator
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
            5
        );
    }
}
