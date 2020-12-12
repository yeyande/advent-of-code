#[macro_use]
extern crate lazy_static;
use regex::Regex;

fn main() {
    let contents = include_str!("../input.txt");
    let instructions: Vec<&str> = contents.lines().into_iter().collect();
    let solution = manhattan_distance(navigate(instructions));
    println!("{}", solution);
}

fn manhattan_distance((x, y): (i64, i64)) -> i64 {
    x.abs() + y.abs()
}

fn turn_ship(facing: char, direction: char, amount: i64) -> char {
    match direction {
        'L' => {
            match facing {
                'N' => {
                    match amount {
                        90 => 'W',
                        180 => 'S',
                        270 => 'E',
                        360 => 'N',
                        _ => 'I'
                    }
                },
                'E' => {
                    match amount {
                        90 => 'N',
                        180 => 'W',
                        270 => 'S',
                        360 => 'E',
                        _ => 'I'
                    }
                },
                'S' => {
                    match amount {
                        90 => 'E',
                        180 => 'N',
                        270 => 'W',
                        360 => 'S',
                        _ => 'I'
                    }
                },
                'W' => {
                    match amount {
                        90 => 'S',
                        180 => 'E',
                        270 => 'N',
                        360 => 'W',
                        _ => 'I'
                    }
                },
                _ => 'I'
            }
        },
        'R' => {
            match facing {
                'N' => {
                    match amount {
                        90 => 'E',
                        180 => 'S',
                        270 => 'W',
                        360 => 'N',
                        _ => 'I'
                    }
                },
                'E' => {
                    match amount {
                        90 => 'S',
                        180 => 'W',
                        270 => 'N',
                        360 => 'E',
                        _ => 'I'
                    }
                },
                'S' => {
                    match amount {
                        90 => 'W',
                        180 => 'N',
                        270 => 'E',
                        360 => 'S',
                        _ => 'I'
                    }
                },
                'W' => {
                    match amount {
                        90 => 'N',
                        180 => 'E',
                        270 => 'S',
                        360 => 'W',
                        _ => 'I'
                    }
                },
                _ => 'I'
            }
        },
        _ => 'I'
    }
}

fn rotate_waypoint(facing: char, direction: char, amount: i64, (x, y): (i64, i64)) -> (i64, i64) {
    translate_coordinates((x, y), direction, amount)
}

fn translate_coordinates((x, y): (i64, i64), direction: char, amount: i64) -> (i64, i64) {
    match direction {
        'R' => {
            match amount {
                0 => (x, y),
                360 => (x, y),
                180 => (-x, -y),
                90 => (y, -x),
                270 => (-y, x),
                _ => (x, y)
            }
        },
        'L' => {
            match amount {
                0 => (x, y),
                360 => (x, y),
                180 => (-x, -y),
                90 => (-y, x),
                270 => (y, -x),
                _ => (x, y)
            }
        },
        _ => (x, y)
    }
}

fn move_ship((facing, (x, y), waypoint): (char, (i64, i64), (i64, i64)), movement: &str) -> (char, (i64, i64), (i64, i64)){
    lazy_static! {
        static ref FORMAT_PARSER: Regex = Regex::new(r"([NSEWLRF])(\d+)").unwrap();
        static ref WAYPOINT: (i64, i64) = (10, 1);
    }
    let captures: Vec<String> = FORMAT_PARSER
        .captures(movement)
        .unwrap()
        .iter()
        .map(|x| String::from(x.unwrap().as_str()))
        .collect();
    let amount: i64 = captures[2].parse().unwrap();
    let (new_facing, movement_direction): (char, char) = match captures[1].as_str() {
        "N" => (facing, 'N'),
        "E" => (facing, 'E'),
        "S" => (facing, 'S'),
        "W" => (facing, 'W'),
        "L" => (turn_ship(facing, 'L', amount), turn_ship(facing, 'L', amount)),
        "R" => (turn_ship(facing, 'R', amount), turn_ship(facing, 'R', amount)),
        "F" => (facing, facing),
        _ => ('I', 'I'),
    };
    let (waypoint_x, waypoint_y) = waypoint;
    if captures[1] == "R" || captures[1] == "L" {
        println!("Rotating");
        print_ship(facing, (x, y), rotate_waypoint(facing, captures[1].chars().next().unwrap(), amount, waypoint));
        return (facing, (x, y), rotate_waypoint(facing, captures[1].chars().next().unwrap(), amount, waypoint));
    }
    if captures[1] == "F" {
        print_ship(new_facing, (x+(waypoint_x*amount), y+(waypoint_y*amount)), waypoint);
        return (new_facing, (x+(waypoint_x*amount), y+(waypoint_y*amount)), waypoint);
    }
    let new_waypoint_position = match movement_direction {
        'N' => (waypoint_x, waypoint_y+amount),
        'E' => (waypoint_x+amount, waypoint_y),
        'S' => (waypoint_x, waypoint_y-amount),
        'W' => (waypoint_x-amount, waypoint_y),
        _ => waypoint
    };
    print_ship(new_facing, (x, y), new_waypoint_position);
    (new_facing, (x, y), new_waypoint_position)
}

fn print_ship(new_facing: char, pos: (i64, i64), waypoint: (i64, i64)) {
    println!("Facing {} at {:?} (waypoint: {:?})", new_facing, pos, waypoint);
}

fn navigate(instructions: Vec<&str>) -> (i64, i64) {
    instructions.iter().fold(('E', (0, 0), (10, 1)), |current, instruction| move_ship(current, instruction)).1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nagivate_should_work_with_input() {
        assert_eq!(
            navigate(vec![
                "F10",
                "N3",
                "F7",
                "R90",
                "F11",
            ]),
            (214, -72)
        );
    }
}
