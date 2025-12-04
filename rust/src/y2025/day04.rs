use crate::Puzzle;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Coordinate {
    x: i32,
    y: i32
}

impl Coordinate {

    fn surrounding(&self) -> [Coordinate; 8] {
        [
            // top
            Coordinate { x: self.x - 1, y: self.y - 1 },
            Coordinate { x: self.x - 1, y: self.y },
            Coordinate { x: self.x - 1, y: self.y + 1 },
            // center
            Coordinate { x: self.x, y: self.y - 1 },
            Coordinate { x: self.x, y: self.y + 1 },
            // bottom
            Coordinate { x: self.x + 1, y: self.y - 1 },
            Coordinate { x: self.x + 1, y: self.y },
            Coordinate { x: self.x + 1, y: self.y + 1 },
        ]
    }

}

fn parse_map(input: String) -> Vec<Vec<bool>> {
    let mut coords = Vec::new();

    for line in input.lines() {
        coords.push(line.chars().map(|c| c == '@').collect());
    }

    coords
}

fn parse(input: String) -> Vec<Coordinate> {
    let mut coords = Vec::new();

    let mut x = 0;
    for line in input.lines() {
        let mut y = 0;
        for c in line.chars() {
            if c == '@' {coords.push(Coordinate { x, y }); }
            y += 1;
        }
        x += 1;
    }

    coords
}

fn part1(rolls: &Vec<Coordinate>) -> u32 {
    let mut count = 0;

    for roll in rolls {
        let surrounding = roll.surrounding().iter()
            .filter(|c| rolls.contains(c))
            .count();
        if surrounding < 4 { count += 1 }
    }

    count
}

fn inaccessible(rolls: &Vec<Coordinate>) -> Vec<Coordinate> {
    let mut inaccessible = Vec::new();

    for roll in rolls {
        let surrounding = roll.surrounding().iter()
            .filter(|c| rolls.contains(c))
            .count();
        if surrounding >= 4 { inaccessible.push(*roll); }
    }

    inaccessible
}

fn part2(rolls: &Vec<Coordinate>) -> usize {
    let mut leftover = rolls.clone();
    let mut changed = 0;

    loop {
        let inaccessible = inaccessible(&leftover);
        let change = leftover.len() - inaccessible.len();

        if change > 0 {
            changed += change;
            leftover = inaccessible;
        } else { break; }
    }

    changed
}

fn run(input: String) {
    let rolls = parse(input);

    println!("Part 1: {}\n Part 2: {}", part1(&rolls), part2(&rolls));
}

pub const PUZZLE: Puzzle = Puzzle { runner: run };
