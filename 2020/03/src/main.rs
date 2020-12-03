fn main() {
    let contents = include_str!("../input.txt");
    let map: Vec<&str> = contents.lines().into_iter().collect();
    let result: usize = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|&x| get_trees_hit_down_slope(map.clone(), x))
        .product();
    println!("{}", result);
}

fn get_trees_hit_down_slope(map: Vec<&str>, slope: (usize, usize)) -> usize {
    let mut pos: (usize, usize) = (0, 0);
    let modulo: usize = map[0].len();
    let (run, rise) = slope;
    let mut trees_hit = 0;
    while pos.1 < map.len() - 1 {
        let (x, y) = pos;
        pos = ((x + run) % modulo, y + rise);
        if map[pos.1].chars().nth(pos.0) == Some('#') {
            trees_hit += 1;
        }
    }
    trees_hit
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! input_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                assert_eq!(
                    expected,
                    get_trees_hit_down_slope(
                        vec![
                            "..##.......",
                            "#...#...#..",
                            ".#....#..#.",
                            "..#.#...#.#",
                            ".#...##..#.",
                            "..#.##.....",
                            ".#.#.#....#",
                            ".#........#",
                            "#.##...#...",
                            "#...##....#",
                            ".#..#...#.#",
                        ],
                        input
                    )
                );
            }
        )*
        }
    }
    input_tests! {
        test_1: ((3,1), 7),
        test_2: ((1,1), 2),
        test_3: ((5,1), 3),
        test_4: ((7,1), 4),
        test_5: ((1,2), 2),
    }
}
