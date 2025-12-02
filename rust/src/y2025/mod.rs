use std::collections::HashMap;
use crate::{Calendar, Puzzle};
use crate::year::Year;

mod day01;
mod day02;

mod template;

pub struct Y25;
impl Calendar for Y25 {
    fn year(&self) -> Year { Year::_2025 }

    fn days(&self) -> HashMap<u8, Puzzle> {
        HashMap::from([
            (1, day01::get_runner()),
            (2, day02::get_runner()),
        ])
    }
}
