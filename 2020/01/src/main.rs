use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").unwrap();
    let values : Vec<i32> = contents
        .lines().into_iter().map(|x| x.parse().unwrap()).collect();
    let (x, y, z) = get_pairs_with_sum(values, 2020);
    println!("{}", x*y*z);
}

fn get_pairs_with_sum(haystack: Vec<i32>, needle: i32) -> (i32, i32, i32) {
    let mut sorted: Vec<i32> = haystack.clone();
    sorted.sort();
    let mut answer: (i32, i32, i32) = (0, 0, 0);
    for x in sorted.clone() {
        for y in sorted.clone()[1..].to_vec() {
            for z in sorted.clone()[2..].to_vec() {
                if x + y + z == needle {
                    answer = (x, y, z)
                }
            }
        }
    }
    return answer;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(get_pairs_with_sum(vec![1721, 979, 366, 299, 675, 1456], 2020), (979, 366, 675))
    }
}
