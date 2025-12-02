use std::collections::HashSet;
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

fn repeat(s: &String, x: usize) -> u64 {
    s.repeat(x).parse().unwrap()
}

fn next(s: &String) -> String {
    let value: u64 = s.parse().unwrap();
    (value + 1).to_string()
}

fn get_repeats(range: &Range, n: usize) -> HashSet<u64> {
    let mut repeats = HashSet::new();

    let (start_string, end_string) = range.to_strings();
    let length = start_string.len();
    let mut current = start_string[..(length / n)].to_string();

    if length % n != 0 {
        if length == end_string.len() {
            // there will be no repeats
            return repeats
        }

        current = "1".to_string();
        current.push_str("0".repeat(length / n).as_str());
    }


    loop {
        let candidate = repeat(&current, n);
        if candidate > range.end { break }

        if candidate > range.start {
            repeats.insert(candidate);
        }
        current = next(&current);
    }

    repeats
}

fn part1(ranges: &Vec<Range>) -> u64 {
    let mut repeats: HashSet<u64> = HashSet::new();

    for range in ranges {
        repeats.extend(get_repeats(range, 2).into_iter());
    }

    repeats.iter().sum()
}

fn part2(ranges: &Vec<Range>) -> u64 {
    let mut repeats: HashSet<u64> = HashSet::new();

    for range in ranges {
        for i in 2..=range.end.to_string().len() {
            repeats.extend(get_repeats(range, i).into_iter());
        }
    }

    repeats.iter().sum()
}

fn run(input: String) {
    let ranges = parse(input);
    println!("Part 1: {}\nPart 2: {}", part1(&ranges), part2(&ranges));
}

pub const PUZZLE: Puzzle = Puzzle { runner: run };
