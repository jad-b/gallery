#[cfg(test)]

// TODO How to use any generic number for function argument?
// RFC=https://github.com/aturon/rfcs/blob/num-reform/active/0000-num-reform.md
// Perhaps specifying the int literal type, such as 0u32?
pub mod numeric {
    use std::iter::{Iterator,Sum};

    /* Iterataive Fibonacci number generator */
    pub fn fibonacci(n: i64) -> i64 {
        let mut a = 1;
        let mut b = 0;
        let mut tmp;
        let mut i = n;
        while i > 0 {
            tmp = a;
            a = a + b;
            b = tmp;
            i = i - 1;
        }
        b
    }

    pub fn sigma<I, T, F>(iter: I, func: F) -> T
        where I: Iterator<Item=T>,
              T: Sum,
              F: FnMut(T) -> T
    {
        // Sum over the values returned by the Iterator, applying our function at each step
        return iter.map(func).sum()
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

    mod tests {
        use numeric;

        #[test]
        fn test_fibonacci()  {
            assert_eq!(numeric::fibonacci(0), 0);
            assert_eq!(numeric::fibonacci(1), 1);
            for i in 2..50 {
                assert_eq!(numeric::fibonacci(i), numeric::fibonacci(i-1) + numeric::fibonacci(i-2));
            }
        }

        #[test]
        fn test_sigma() {
            // Closure types differ when defined _outside_ of a function argument
            // vs. on their own line, using `let`
            assert_eq!(55, numeric::sigma(1..11, |x| x))
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

}
