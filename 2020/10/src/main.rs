fn main() {
    let contents = include_str!("../input.txt");
    let adapters: Vec<u32> = contents
        .lines()
        .into_iter()
        .map(|l| l.parse().unwrap())
        .collect();
    let solution = get_jolt_differences(adapters.clone());
    println!("{}", solution.0 * solution.2);
    let solution2 = get_adapter_combinations(adapters);
    println!("{}", solution2)
}

fn add_jolt_difference(
    (one, two, three): (u32, u32, u32),
    current: u32,
    previous: u32,
) -> (u32, u32, u32) {
    match current - previous {
        1 => (one + 1, two, three),
        2 => (one, two + 1, three),
        3 => (one, two, three + 1),
        _ => (one, two, three),
    }
}

fn get_jolt_differences(adapters: Vec<u32>) -> (u32, u32, u32) {
    let mut sorted = adapters;
    sorted.sort_unstable();
    sorted
        .iter()
        .enumerate()
        .fold((0, 0, 1), |acc, (idx, jolt)| match idx {
            0 => add_jolt_difference(acc, *jolt, 0),
            _ => add_jolt_difference(
                acc,
                *jolt,
                *sorted.clone().get(idx - 1).unwrap(),
            ),
        })
}

fn get_paths(adapters: Vec<u32>) -> i64 {
    let mut paths: Vec<i64> = vec![1];
    for (idx, x) in adapters.iter().enumerate() {
        for next_idx in idx + 1..idx + 4 {
            if let Some(n) = adapters.get(next_idx) {
                if *n - x <= 3 {
                    match paths.get(next_idx) {
                        Some(p) => paths[next_idx] = paths[idx] + p,
                        None => paths.insert(next_idx, paths[idx]),
                    }
                }
            }
        }
    }
    paths[paths.len() - 1] as i64
}

fn get_adapter_combinations(adapters: Vec<u32>) -> i64 {
    let mut sorted = adapters;
    sorted.sort_unstable();
    sorted.insert(0, 0);
    sorted.push(sorted.iter().max().unwrap() + 3);
    get_paths(sorted)
}

#[cfg(test)]
mod tests {
    use super::*;
    static TEST_INPUT_1: [u32; 11] = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4];
    static TEST_INPUT_2: [u32; 31] = [
        28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8,
        17, 7, 9, 4, 2, 34, 10, 3,
    ];

    #[test]
    fn get_jolt_differences_should_work_for_sample_input() {
        assert_eq!(get_jolt_differences(TEST_INPUT_1.to_vec()), (7, 0, 5),);
        assert_eq!(get_jolt_differences(TEST_INPUT_2.to_vec()), (22, 0, 10));
    }

    #[test]
    fn get_adapter_combinations_should_work_for_sample_input() {
        assert_eq!(get_adapter_combinations(TEST_INPUT_1.to_vec()), 8,);
        assert_eq!(get_adapter_combinations(TEST_INPUT_2.to_vec()), 19208);
    }
}
