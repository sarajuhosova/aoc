use crate::Puzzle;

struct Range {
    start: u64,
    end: u64
}

impl Range {

    fn to_strings(&self) -> (String, String) {
        (self.start.to_string(), self.end.to_string())
    }

}

fn parse(input: String) -> Vec<Range> {
    input.split(',')
        .map(|part| {
            let split = part.split_once('-');
            match split {
                None => panic!("Invalid range"),
                Some((fst, rest)) => Range {
                    start: fst.parse().unwrap(),
                    end: rest.parse().unwrap()
                }
            }
        }).collect::<Vec<Range>>()
}

fn double(s: &String) -> u64 {
    let mut doubled = s.to_owned();
    doubled.push_str(s);
    doubled.parse().unwrap()
}

fn next(s: &String) -> String {
    let value: u64 = s.parse().unwrap();
    (value + 1).to_string()
}

fn get_repeats(range: &Range) -> Vec<u64> {
    let mut repeats = Vec::new();

    let (start_string, end_string) = range.to_strings();
    let length = start_string.len();
    let mut current = start_string[..length/2].to_string();

    if length % 2 != 0 {
        if length == end_string.len() {
            // there will be no repeats
            return repeats
        }

        current = "1".to_string();
        current.push_str("0".repeat(length / 2).as_str());
    }


    loop {
        let candidate = double(&current);
        if candidate > range.end { break }

        if candidate > range.start {
            repeats.push(candidate);
        }
        current = next(&current);
    }

    repeats
}

fn part1(ranges: &Vec<Range>) -> u64 {
    let mut sum: u64 = 0;

    for range in ranges {
        let repeats: Vec<u64> = get_repeats(range);
        sum += repeats.iter().sum::<u64>();
    }

    sum
}

fn part2(ranges: &Vec<Range>) -> u64 {
    0
}

fn run(input: String) {
    let ranges = parse(input);
    println!("Part 1: {}\nPart 2: {}", part1(&ranges), part2(&ranges));
}

pub fn get_runner() -> Puzzle { run }
