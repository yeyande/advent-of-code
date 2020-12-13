fn main() {
    let contents = include_str!("../input.txt");
    let schedule: Vec<&str> = contents.lines().into_iter().collect();
    let solution = solve(schedule);
    println!("{}", solution);
}

fn get_earliest_bus_id(earliest_depart: usize, bus_ids: Vec<usize>) -> (usize, usize) {
    for depart_time in earliest_depart.. {
        for id in bus_ids.iter() {
            if depart_time % id == 0 {
                return (*id, depart_time);
            }
        }
    }
    (0, 0)
}

fn solve(schedule: Vec<&str>) -> usize {
    let depart_time: usize = schedule[0].parse().unwrap();
    let bus_ids: Vec<usize> = schedule[1].split(",").into_iter().filter_map(|id| id.parse().ok()).collect();
    let (next_bus_id, arrival_time): (usize, usize) = get_earliest_bus_id(depart_time, bus_ids);
    next_bus_id*(arrival_time - depart_time)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_earliest_bus_id_should_work_with_input() {
        assert_eq!(
            get_earliest_bus_id(939, vec![7,13,59,31,19]),
            (59, 944)
        );
    }
    #[test]
    fn solve_should_work_with_input() {
        assert_eq!(
            solve(vec![
                "939",
                "7,13,x,x,59,x,31,19",
            ]),
            295
        );
    }
}
