fn main() {
    let contents = include_str!("../input.txt");
    let layout: Vec<&str> = contents
        .lines()
        .into_iter()
        .collect();
    let solution = solve(layout.clone());
    println!("{}", solution);
}

fn simulate_arrivals(layout: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut post_sit_layout : Vec<Vec<char>> = layout.clone();
    for (row_idx, row) in layout.iter().enumerate() {
        for (col_idx, _) in row.iter().enumerate() {
            post_sit_layout[row_idx][col_idx] = sit_if_available(layout.clone(), (row_idx, col_idx));
        }
    }
    let mut post_stand_layout : Vec<Vec<char>> = post_sit_layout.clone();
    for (row_idx, row) in post_sit_layout.clone().iter().enumerate() {
        for (col_idx, _) in row.iter().enumerate() {
            post_stand_layout[row_idx][col_idx] = get_up_if_needed(post_sit_layout.clone(), (row_idx, col_idx));
        }
    }
    post_stand_layout
}

fn get_up_if_needed(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> char {
    match should_get_up(layout.clone(), (row, col)) {
        true => 'L',
        false => layout[row][col]
    }
}

fn get_seat_at_location(layout: Vec<Vec<char>>, (row, col): (Option<usize>, Option<usize>)) -> char {
    // base case: it exists!
    // If we're to the left of the table, we should return a .
    match row {
        None => 'L',
        Some(r) => {
            match r >= layout.len() {
                true => 'L',
                false => {
                    match col {
                        None => 'L',
                        Some(c) => {
                            match c >= layout[r].len() {
                                true => 'L',
                                false => layout[r][c]
                            }
                        }
                    }
                }
            }
        }
    }
}

fn get_adjacent_seats(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> Vec<char> {
    [
      (row.checked_sub(1), col.checked_sub(1)), // layout[row-1][col-1]
      (row.checked_sub(1), col.checked_sub(0)), // layout[row-1][col  ]
      (row.checked_sub(1), col.checked_add(1)), // layout[row-1][col+1]
      (row.checked_sub(0), col.checked_sub(1)), // layout[row  ][col-1]
      (row.checked_sub(0), col.checked_add(1)), // layout[row  ][col+1]
      (row.checked_add(1), col.checked_sub(1)), // layout[row+1][col-1]
      (row.checked_add(1), col.checked_add(0)), // layout[row+1][col  ]
      (row.checked_add(1), col.checked_add(1))  // layout[row+1][col+1]
    ].iter().map(|pos| get_seat_at_location(layout.clone(), *pos)).collect()
}

fn get_first_seat(layout: Vec<Vec<char>>, (row, col): (usize, usize), dir: (i32, i32)) -> char {
    get_first_seat_helper(layout, (Some(row), Some(col)), dir)
}

fn get_first_seat_helper(layout: Vec<Vec<char>>, (row, col): (Option<usize>, Option<usize>), (run, rise): (i32, i32)) -> char {
    match row {
        None => 'L',
        Some(r) => {
            match col {
                None => 'L',
                Some(c) => {
                    let new_row = match run.is_positive() {
                        true => r.checked_add(run as usize),
                        false => r.checked_sub(run.abs() as usize),
                    };
                    let new_col = match rise.is_positive() {
                        true => c.checked_add(rise as usize),
                        false => c.checked_sub(rise.abs() as usize),
                    };
                    let seat: char = get_seat_at_location(layout.clone(), (new_row, new_col));
                    match seat {
                        '#' => seat,
                        'L' => seat,
                        _ => get_first_seat_helper(layout, (new_row, new_col), (run, rise)),
                    }
                }
            }
        }
    }
}

fn get_seen_seats(layout:Vec<Vec<char>>, position: (usize, usize)) -> Vec<char> {
    [ (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1),
    ].iter().map(|dir| get_first_seat(layout.clone(), position, *dir)).collect()
}

fn should_get_up(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> bool {
    let mut checks: Vec<bool> = vec![];
    checks.push(layout[row][col] == '#');
    let adjacent_seats: Vec<char> = get_seen_seats(layout.clone(), (row, col));
    let occupied_seats: Vec<&char> = adjacent_seats.iter().filter(|seat| **seat == '#').collect();
    checks.push(occupied_seats.len() >= 5);
    checks.into_iter().fold(true, |a, b| a&&b)
}

fn sit_if_available(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> char {
    match should_sit(layout.clone(), (row, col)) {
        true => '#',
        false => layout[row][col]
    }
}

fn should_sit(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> bool {
    let mut checks: Vec<bool> = vec![];
    checks.push(layout[row][col] == 'L');
    let adjacent_seats: Vec<char> = get_seen_seats(layout.clone(), (row, col));
    let occupied_seats: Vec<&char> = adjacent_seats.iter().filter(|seat| **seat == '#').collect();
    checks.push(occupied_seats.len() == 0);
    checks.into_iter().fold(true, |a, b| a&&b)
}

fn print_layout(layout : Vec<Vec<char>>) {
    for row in layout {
        let as_str: String = row.into_iter().collect();
        println!("{}", as_str);
    }
    println!("");
}

fn solve(lobby: Vec<&str>) -> usize {
    let mut layout: Vec<Vec<char>> = lobby.clone().iter().map(|x| x.chars().collect()).collect();
    loop {
        let new_layout = simulate_arrivals(layout.clone());
        match layout == new_layout {
            true => break,
            false => layout = new_layout,
        }
    }
    layout.iter().fold(0, |a,b| a + b.iter().filter(|x| **x == '#').count())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_work_for_sample_input() {
        assert_eq!(
            solve(vec![
                "L.LL.LL.LL",
                "LLLLLLL.LL",
                "L.L.L..L..",
                "LLLL.LL.LL",
                "L.LL.LL.LL",
                "L.LLLLL.LL",
                "..L.L.....",
                "LLLLLLLLLL",
                "L.LLLLLL.L",
                "L.LLLLL.LL",
            ]),
            26
        );
    }
}
