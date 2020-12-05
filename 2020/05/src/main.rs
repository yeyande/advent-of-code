fn main() {
    let contents = include_str!("../input.txt");
    let solution = solve(contents.lines().into_iter().collect());
    println!("{}", solution);
}

fn solve(seats: Vec<&str>) -> u32 {
    let seat_ids: Vec<u32> = seats.iter().map(|x| get_seat_id(get_seat(x))).collect();
    *seat_ids.iter().max().unwrap()
}

fn get_seat_id((row, col): (u32, u32)) -> u32 {
    row * 8 + col
}

fn get_seat(instructions: &str) -> (u32, u32) {
    let mut row = (0, 127);
    let mut col = (0, 7);
    let mut take_first = (true, true);
    for i in instructions.chars() {
        let (row_lower, row_upper) = row;
        let (col_lower, col_upper) = col;
        let (take_first_row, take_first_col) = take_first;
        match i {
            'F' => {
                row = (row_lower, (row_upper + row_lower)/ 2);
                take_first = (true, take_first_col);
            },
            'B' => {
                row = ((row_lower + row_upper + 1) / 2, row_upper);
                take_first = (false, take_first_col);
            },
            'R' => {
                col = ((col_lower + col_upper + 1) / 2, col_upper);
                take_first = (take_first_row, false);
            },
            'L' => {
                col = (col_lower, (col_upper + col_lower)/2);
                take_first = (take_first_row, true);
            },
            _ => {}
        }
    }
    match take_first {
        (true, true) => (row.0, col.0),
        (true, false) => (row.0, col.1),
        (false, true) => (row.1, col.0),
        (false, false) => (row.1, col.1)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_on_sample_input() {
        assert_eq!(solve(vec!["FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]), 820)
    }

    #[test]
    fn get_seat_should_work() {
        assert_eq!(get_seat("FBFBBFFRLR"), (44, 5));
        assert_eq!(get_seat("BFFFBBFRRR"), (70, 7));
        assert_eq!(get_seat("FFFBBBFRRR"), (14, 7));
        assert_eq!(get_seat("BBFFBBFRLL"), (102, 4));
    }

    #[test]
    fn get_seat_id_should_work() {
        assert_eq!(get_seat_id((44, 5)), 357);
        assert_eq!(get_seat_id((70, 7)), 567);
        assert_eq!(get_seat_id((14, 7)), 119);
        assert_eq!(get_seat_id((102, 4)), 820);
    }
}
