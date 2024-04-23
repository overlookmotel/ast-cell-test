/// Const fn to get maximum value in a slice of `usize`s
pub const fn max_all(values: &[usize]) -> usize {
    let mut max = 0;
    let mut i = 0;
    loop {
        if i == values.len() {
            break;
        }
        if values[i] > max {
            max = values[i];
        }
        i += 1;
    }
    max
}
