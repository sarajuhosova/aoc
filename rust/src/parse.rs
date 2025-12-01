pub fn by_line<T>(f: fn(&str) -> T, input: String) -> Vec<T> {
    input.lines().map(f).collect::<Vec<T>>()
}

pub fn to_ints(input: String) -> Vec<i32> {
    by_line(|line| line.parse::<i32>().unwrap(), input)
}

pub fn to_longs(input: String) -> Vec<i64> {
    by_line(|line| line.parse::<i64>().unwrap(), input)
}
