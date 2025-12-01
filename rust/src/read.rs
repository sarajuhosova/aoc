use std::fs;
use crate::year::Year;

pub fn read(year: Year, day: u8, filename: &str) -> String {
    fs::read_to_string(
        format!("resources/{}/{}.txt", year.to_directory(day), filename)
    ).expect("Should have been able to read the file").trim().to_owned()
}
