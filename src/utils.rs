pub fn strtol(s: &str) -> (Option<i64>, &str) {
    let mut nr = "".to_string();
    for (idx, c) in s.char_indices() {
        if !c.is_ascii_digit() {
            let d = nr.parse::<i64>().ok();
            let rest = &s[idx..];
            return (d, rest);
        }

        nr.push(c);
    }

    let d = nr.parse::<i64>().ok();
    (d, "")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strtol_parse() {
        assert_eq!((Some(5), "+20-4"), strtol("5+20-4"));
        assert_eq!((None, "+20-4"), strtol("+20-4"));
        assert_eq!((Some(20), "-4"), strtol("20-4"));
        assert_eq!((None, "-4"), strtol("-4"));
        assert_eq!((Some(4), ""), strtol("4"));
        assert_eq!((None, ""), strtol(""));
    }
}
