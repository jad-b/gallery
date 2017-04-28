#[cfg(test)]
mod tests {
    use numeric;

    #[test]
    fn test_sigma() {
        // Closure types differ when defined _outside_ of a function argument
        // vs. on their own line, using `let`
        assert_eq!(55, numeric::sigma(|x| x, 1, |x| x + 1, 10))
    }

    #[test]
    fn gcd_test() {
        assert_eq!(numeric::gcd(206, 40), 2);
    }

    #[test]
    fn gcd_iter_test() {
        assert_eq!(numeric::gcd_iter(206, 40), 2);
    }
}


// TODO How to use any generic number for function argument?
// RFC=https://github.com/aturon/rfcs/blob/num-reform/active/0000-num-reform.md
// Perhaps specifying the int literal type, such as 0u32?
pub mod numeric {

    /* This is a stupid implementation; a better one would accept `term` and
     * an iterator that generates our [a:b] range.
     */
    pub fn sigma<F,G>(term: F, a: i32, next: G, b: i32) -> i32
        where F: Fn(i32) -> i32, G: Fn(i32) -> i32 {
        match a > b {
            true => 0i32,
            false => a + sigma(term, next(a), next, b),
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

}
