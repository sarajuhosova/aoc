#[derive(Debug)]
pub enum Year {
    _2024,
    _2025
}

impl Year {

    pub fn to_directory(&self, day: u8) -> String {
        let max_day = if matches!(self, Year::_2024) { 25 } else { 12 };

        if day < 1 || day > max_day {
            panic!("Day must be between 1 and {max_day}");
        }

        let year = match self {
            Year::_2024 => "y2024",
            Year::_2025 => "y2025"
        };

        format!("{}/day{:02}", year, day)
    }

}
