use crate::Puzzle;

fn parse(input: &str) -> Vec<u8> {
    input.chars().map(|c| c.to_digit(10).unwrap() as u8).collect()
}

fn max(input: &Vec<u8>, skip: u8) -> (u8, usize) {
    let mut largest = (0, 0);
    for (i, x) in input.iter().enumerate() {
        // skip the last one
        if (i + skip as usize) >= input.len() { break }
        // add this index
        if x > &largest.0 {
            largest = (x.to_owned(), i);
        }
    }
    largest
}

fn get_joltage(bank: &Vec<u8>, size: u8) -> u64 {
    let mut remaining = bank.clone();
    let mut result: u64 = 0;

    for i in (0..size).rev() {
        result *= 10;
        let (value, index) = max(&remaining, i);
        result += value as u64;
        remaining = remaining[index + 1..].to_vec();
    }

    result
}

fn turn_on(banks: &Vec<Vec<u8>>, size: u8) -> u64 {
    banks.iter().map(|bank| get_joltage(bank, size)).sum()
}

fn run(input: String) {
    let banks = input.lines().map(|l| parse(&l)).collect::<Vec<_>>();

    println!("Part 1: {}\nPart 2: {}", turn_on(&banks, 2), turn_on(&banks, 12));
}

pub const PUZZLE: Puzzle = Puzzle { runner: run };
