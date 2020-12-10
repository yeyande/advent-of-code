fn main() {
    let contents = include_str!("../input.txt");
    let adapters: Vec<u32> = contents.lines().into_iter().map(|l| l.parse().unwrap()).collect();
    let solution = get_jolt_differences(adapters);
    println!("{}", solution.0*solution.2)
}

fn add_jolt_difference((one, two, three): (u32, u32, u32), current: u32, previous: u32) -> (u32, u32, u32) {
    match current - previous {
        1 => (one + 1, two, three),
        2 => (one, two + 1, three),
        3 => (one, two, three + 1),
        _ => (one, two, three)
    }
}

fn get_jolt_differences(adapters: Vec<u32>) -> (u32, u32, u32) {
    let mut sorted = adapters.clone();
    sorted.sort();
    sorted
        .iter()
        .enumerate()
        .fold((0, 0, 1), |acc, (idx, jolt)| 
              match idx {
                  0 => add_jolt_difference(acc.clone(), jolt.clone(), 0),
                  _ => add_jolt_difference(acc.clone(), jolt.clone(), sorted.clone().get(idx-1).unwrap().clone())
              }
              )
}

fn get_paths(current: u32, adapters: Vec<u32>, index: usize) -> usize {
    println!("{:?}", adapters);
    let bounds = match 3.cmp(&(adapters.len() - index - 1)) {
            std::cmp::Ordering::Greater => adapters.len() - index,
            std::cmp::Ordering::Less => 4,
            std::cmp::Ordering::Equal => 4,
    };
    println!("{}", bounds);
    match bounds {
        1 => 1,
        _ => {
            let valid_numbers: Vec<&u32> = adapters[index+1..index+bounds].iter().filter(|x| **x <=3 || current >= *x-3).collect();
            valid_numbers.len()
        }
    }
}

fn get_adapter_combinations(adapters: Vec<u32>) -> usize {
    let mut sorted = adapters.clone();
    sorted.sort();
    sorted.insert(0, 0);
    sorted.push(sorted.iter().max().unwrap()+3);
    let paths: Vec<usize> = sorted.iter().enumerate().map(|(idx, jolt)| get_paths(*jolt, sorted.clone(), idx)).collect();
    println!("{:?}", paths);
    paths.into_iter().filter(|x| *x != 0).fold(1, |a, b| a*b)/2
}

#[cfg(test)]
mod tests {
    use super::*;
    static TEST_INPUT_1: [u32; 11] = [
                16,
                10,
                15,
                5,
                1,
                11,
                7,
                19,
                6,
                12,
                4,
            ];
    static TEST_INPUT_2: [u32; 31] = [
                28,
                33,
                18,
                42,
                31,
                14,
                46,
                20,
                48,
                47,
                24,
                23,
                49,
                45,
                19,
                38,
                39,
                11,
                1,
                32,
                25,
                35,
                8,
                17,
                7,
                9,
                4,
                2,
                34,
                10,
                3,
            ];

    #[test]
    fn get_jolt_differences_should_work_for_sample_input() {
        assert_eq!(
            get_jolt_differences(TEST_INPUT_1.to_vec()),
            (7, 0, 5),
        );
        assert_eq!(
            get_jolt_differences(TEST_INPUT_2.to_vec()),
            (22, 0, 10)
        );
    }

    #[test]
    fn get_adapter_combinations_should_work_for_sample_input() {
        assert_eq!(
            get_adapter_combinations(TEST_INPUT_1.to_vec()),
            8,
        );
        assert_eq!(
            get_adapter_combinations(TEST_INPUT_2.to_vec()),
            19208
        );
    }
}
