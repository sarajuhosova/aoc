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
    let year: Year = Year::_2025;
    let day: u8 = 1;
    let filename: &str = "in";

    match year {
        Year::_2024 => { y2024::Y24.run(day, filename) },
        Year::_2025 => { y2025::Y25.run(day, filename) }
    }
}
