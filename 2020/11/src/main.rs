fn main() {
    println!("Hello, world!");
}

fn simulate_arrivals(layout: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_layout : Vec<Vec<char>> = layout.clone();
    for (row_idx, row) in layout.iter().enumerate() {
        for (col_idx, _) in row.iter().enumerate() {
            new_layout[row_idx][col_idx] = sit_if_available(layout.clone(), (row_idx, col_idx));
        }
    }
    for (row_idx, row) in new_layout.clone().iter().enumerate() {
        for (col_idx, _) in row.iter().enumerate() {
            new_layout[row_idx][col_idx] = get_up_if_needed(new_layout.clone(), (row_idx, col_idx));
        }
    }
    new_layout
}

fn get_up_if_needed(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> char {
    match should_get_up(layout.clone(), (row, col)) {
        true => 'L',
        false => layout[row][col]
    }
}

fn should_get_up(layout: Vec<Vec<char>>, (row, col): (usize, usize)) -> bool {
    let mut checks: Vec<bool> = vec![];
    checks.push(layout[row][col] == '#');
    let search_rows = row.saturating_sub(4)..*[row.saturating_add(4), layout.len()].iter().min().unwrap();
    checks.push(layout[search_rows]
                .into_iter()
                .filter(|r| r[col] == '#' ).collect::<Vec<Vec<char>>>().len() >= 5);
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
    if col != 0 {
        checks.push(layout[row][col-1] != '#');
    }
    if col < layout[row].len() - 1 {
        checks.push(layout[row][col+1] != '#');
    }
    if row != 0 {
        checks.push(layout[row-1][col] != '#');
    }
    if row < layout.len() - 1 {
        checks.push(layout[row+1][col] != '#');
    }
    checks.into_iter().fold(true, |a, b| a&&b)
}

fn solve(lobby: Vec<&str>) -> usize {
    let mut layout: Vec<Vec<char>> = lobby.clone().iter().map(|x| x.chars().collect()).collect();
    println!("{:?}", layout);
    loop {
        let new_layout = simulate_arrivals(layout.clone());
        println!("");
        println!("{:?}", new_layout);
        match layout == new_layout {
            true => break,
            false => layout = new_layout,
        }
    }
    lobby.iter().fold(0, |a,b| a + b.matches("L").count())
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
            37
        );
    }
}
