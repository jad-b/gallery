#[cfg(test)]
mod tests {
    use gcd; // Imports our function
    use gcd_iter;

    #[test]
    fn gcd_test() {
        assert_eq!(gcd(206, 40), 2);
    }

    #[test]
    fn gcd_iter_test() {
        assert_eq!(gcd_iter(206, 40), 2);
    }
}

pub fn gcd(a: u32, b: u32) -> u32 {
    if let 0 = b {
        a
    } else {
        gcd(b, a % b)
    }
}

pub  fn gcd_iter(a: u32, b: u32) ->  u32 {
    let (mut x, mut y) = (a, b);
    while !(y == 0) {
        let r = x % y;
        x = y;
        y = r;
    }
    x
}
