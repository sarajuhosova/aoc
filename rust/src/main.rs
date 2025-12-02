use crate::year::Year;
use std::collections::HashMap;

mod y2024;
mod y2025;

pub mod read;
pub mod year;
mod parse;

type Puzzle = fn(String) -> ();

pub trait Calendar {
    fn year(&self) -> Year;

    fn days(&self) -> HashMap<u8, Puzzle>;

    fn run(&self, day: u8, filename: &str) {
        match self.days().get(&day) {
            Some(f) => {
                let input: String = read::read(self.year(), day, filename);
                f(input)
            },
            None => println!("Day {} of year {:?} not implemented", day, self.year()),
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 4 {
        panic!("Please provide the year, day, and filename as arguments");
    }

    let mut iter = args.iter().skip(1);

    let filename: &str = iter.next().unwrap();
    let year: Year = Year::from_string(iter.next().unwrap());
    let day: u8 = iter.next().unwrap().parse().unwrap();

    match year {
        Year::_2024 => { y2024::Y24.run(day, filename) },
        Year::_2025 => { y2025::Y25.run(day, filename) }
    }
}
