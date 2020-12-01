use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").unwrap();
    let values : Vec<i32> = contents
        .lines().into_iter().map(|x| x.parse().unwrap()).collect();
    let (x, y) = get_pairs_with_sum(values, 2020);
    println!("{}", x*y);
}

fn get_pairs_with_sum(haystack: Vec<i32>, needle: i32) -> (i32, i32) {
    let mut sorted: Vec<i32> = haystack.clone();
    sorted.sort();
    let (left, right): (Vec<i32>, Vec<i32>) = sorted.iter().partition(|&&x| x < needle/2);
    let mut lesser: i32 = 0;
    let mut greater: i32 = 0;
    for x in left {
        for y in right.clone() {
            if x + y == needle {
                lesser = x;
                greater = y;
            }
        }
    }
    return (greater, lesser)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(get_pairs_with_sum(vec![1721, 979, 366, 299, 675, 1456], 2020), (1721, 299))
    }
}
