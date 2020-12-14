use std::thread;
fn main() {
    let contents = include_str!("../input.txt");
    let schedule: Vec<&str> = contents.lines().into_iter().collect();
    let solution = solve(schedule.clone());
    println!("{}", solution);
    let solution_2 = solve_2(schedule);
    println!("{}", solution_2);
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

fn parse_bus((offset, id): (usize, &str)) -> Option<(usize, usize)> {
    match id {
        "x" => None,
        _ => Some((id.parse().unwrap(), offset))
    }
}

fn parse_bus_schedule(schedule: &str) -> Vec<(usize, usize)> {
    schedule.split(",").into_iter().enumerate().filter_map(parse_bus).collect()
}

fn get_parallel(schedule: Vec<(usize, usize)>, range: std::ops::Range<usize>) {
    let initial_bus = schedule[0];
    for x in range {
        let timestamp = initial_bus.0*x;
        if schedule[1..].iter().all(|(interval, offset)| ((timestamp+offset)%interval) == 0) {
            println!("I FOUND IT! It is: {}", timestamp);
        }
    }
}

fn get_earliest_consecutive_timestamp(schedule: Vec<(usize, usize)>) -> usize {
    println!("{:?}", schedule);
    let initial_bus = schedule[0];
    //let start_x = 100000000000000 / initial_bus.0;
    let end_x: usize= schedule.iter().map(|x| x.0).fold(1, |a,b| a*b)/2;
    println!("end_x: {}", end_x);
    let steps = schedule.iter().map(|(id, _)| id).max().unwrap();
    let range: Vec<usize> = (100000000000000..end_x).step_by(*steps).collect();
    let chunks = range.chunks(8);
    let mut threads: Vec<thread::JoinHandle<()>> = vec![];
    for chunk in chunks {
        let tmp_schedule = schedule.clone();
        let tmp_range = chunk[0]..chunk[chunk.len()-1];
        threads.push(thread::spawn(move || get_parallel(tmp_schedule, tmp_range)));
    }
    for thread in threads {
        let  _ = thread.join();
    }

    for x in (100000000000000..end_x).step_by(*steps) {
        let timestamp = initial_bus.0*x;
        if schedule[1..].iter().all(|(interval, offset)| ((timestamp+offset)%interval) == 0) {
            return timestamp
        }
    }
    0
}

fn solve_2(schedule: Vec<&str>) -> usize {
    let bus_ids: Vec<(usize, usize)> = parse_bus_schedule(schedule[1]);
    get_earliest_consecutive_timestamp(bus_ids)
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

    #[test]
    fn solve_2_should_work_with_input() {
        assert_eq!(
            solve_2(vec![
                "939",
                "7,13,x,x,59,x,31,19",
            ]),
            1068781
        );
    }

    #[test]
    fn parse_bus_schedule_should_work_with_input() {
        assert_eq!(
            parse_bus_schedule("17,x,13,19"),
            vec![(17, 0), (13, 2), (19, 3)]
        );
        assert_eq!(
            parse_bus_schedule("7,13,x,x,59,x,31,19"),
            vec![(7, 0), (13, 1), (59, 4), (31, 6), (19, 7)]
        );
    }

    #[test]
    fn get_earliest_consecutive_timestamp_should_work_with_input() {
        assert_eq!(
            get_earliest_consecutive_timestamp(vec![(17, 0),(13, 2),(19, 3)]),
                3417
        );
    }
}
