use std::collections::HashMap;
use crate::{Calendar, Puzzle};
use crate::year::Year;

mod day01;

pub struct Y24;
impl Calendar for Y24 {
    fn year(&self) -> Year { Year::_2024 }

    fn days(&self) -> HashMap<u8, Puzzle> {
        HashMap::from([
            (1, day01::get_runner()),
        ])
    }
}
