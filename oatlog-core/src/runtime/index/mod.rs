#[cfg(test)]
mod proptest;

/// dedup `vec[offset..]`.
pub fn dedup_suffix<T: Copy + PartialOrd>(vec: &mut Vec<T>, offset: usize) {
    assert!(offset <= vec.len());
    let s = &mut vec[offset..];
    if s.len() <= 1 {
        return;
    }
    debug_assert!(s.is_sorted());
    let mut i = 0;
    let mut j = 1;
    loop {
        while j < s.len() && s[i] == s[j] {
            j += 1;
        }
        if j == s.len() {
            break;
        }
        i += 1;
        s[i] = s[j];
    }
    vec.truncate(offset + i + 1);
}
#[cfg(test)]
mod test {
    #[test]
    fn dedup_suffix_smoke() {
        let mut v = vec![10, 10, 2, 2, 3, 4, 4];
        super::dedup_suffix(&mut v, 2);
        assert_eq!(v, &[10, 10, 2, 3, 4]);
    }
    #[test]
    fn dedup_suffix_empty() {
        let mut v: Vec<i32> = vec![];
        super::dedup_suffix(&mut v, 0);
        assert_eq!(v, &[]);
    }
}
