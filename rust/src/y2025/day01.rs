use crate::Puzzle;

const DIAL_SIZE: i32 = 100;
const START_POSITION: i32 = 50;

struct Rotation {
    operation: fn(i32, i32) -> i32,
    value: i32
}

fn parse(line: &str) -> Rotation {
    let operation = match line.chars().nth(0).unwrap() {
        'L' => |a, b| a - b,
        'R' => |a, b| a + b,
        _ => panic!("Invalid rotation")
    };
    let value: i32 = line[1..].parse().unwrap();

    Rotation { operation, value }
}

fn modulate(position: i32) -> i32 {
    ((position % DIAL_SIZE) + DIAL_SIZE) % DIAL_SIZE
}

fn part1(rotations: &Vec<Rotation>) -> i32 {
    let mut count = 0;
    let mut position = START_POSITION;

    for rotation in rotations {
        position = (rotation.operation)(position, rotation.value);
        position = modulate(position);
        if position == 0 { count += 1 }
    }

    count
}

fn part2(rotations: &Vec<Rotation>) -> i32 {
    let mut count = 0;
    let mut position = START_POSITION;

    for rotation in rotations {
        let mut moves = rotation.value;
        count += moves / DIAL_SIZE;
        moves %= DIAL_SIZE;

        position = (rotation.operation)(position, moves);
        if position == 0 { count += 1 }
        else if position < 0 || position > DIAL_SIZE {
            count += 1;
            position = modulate(position);
        }

    }

    count
}

fn run(input: String) {
    let rotations: Vec<Rotation> = input.lines().map(parse).collect::<Vec<Rotation>>();

    println!("Part 1: {}\nPart 2: {}", part1(&rotations), part2(&rotations));
}

pub const PUZZLE: Puzzle = Puzzle { runner: run };
