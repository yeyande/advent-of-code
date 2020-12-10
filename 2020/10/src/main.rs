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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(
            get_jolt_differences(vec![
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
            ]), 
            (7, 0, 5),
        );
        assert_eq!(
            get_jolt_differences(vec![
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
            ]),
            (22, 0, 10)
        );
    }
}
