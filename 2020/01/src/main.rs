use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").unwrap();
    let values : Vec<i32> = contents
        .lines().into_iter().map(|x| x.parse().unwrap()).collect();
    let (x, y, z) = get_triple_with_sum(values, 2020).unwrap();
    println!("{}", x*y*z);
}

fn get_pairs_with_sum(haystack: Vec<i32>, needle: i32) -> Option<(i32, i32)> {
    let mut sorted: Vec<i32> = haystack.clone();
    sorted.sort();
    for x in sorted.clone() {
        for y in sorted.clone()[1..].to_vec() {
            if x + y == needle {
                return Some((x, y));
            }
        }
    }
    None
}

fn get_triple_with_sum(haystack: Vec<i32>, needle: i32) -> Option<(i32, i32, i32)> {
    let mut sorted: Vec<i32> = haystack.clone();
    sorted.sort();
    for x in sorted.clone() {
        match get_pairs_with_sum(sorted.clone(), needle-x) {
            Some((y, z)) => {
                if x + y + z == needle {
                    return Some((x, y, z))
                }
            }
            None => continue
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(get_triple_with_sum(vec![1721, 979, 366, 299, 675, 1456], 2020).unwrap(), (366, 675, 979))
    }
}
