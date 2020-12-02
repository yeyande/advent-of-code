fn main() {
    println!("Hello, world!");
}

fn get_valid_password_count(passwords: Vec<&str>) -> usize {
    return passwords.len();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(get_valid_password_count(vec!["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]), 2)
    }
}
