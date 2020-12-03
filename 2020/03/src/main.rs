fn main() {
    let contents = include_str!("../input.txt");
    let map : Vec<&str> = contents.lines().into_iter().collect();
    println!("{}", get_trees_hit_down_slope(map, (3,1)));
}

fn get_trees_hit_down_slope(map: Vec<&str>, slope: (usize, usize)) -> usize {
    let mut pos : (usize, usize) = (0, 0);
    let modulo: usize = map[0].len();
    let (run, rise) = slope;
    let mut trees_hit = 0;
    while pos.1 < map.len() - 1 {
        let (x, y) = pos;
        pos = ((x+run) % modulo, y+rise);
        if map[pos.1].chars().nth(pos.0) == Some('#') {
            trees_hit += 1;
        }
    }
    return trees_hit;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(
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
                (3,1)
                ),
            7
        )
    }
}
