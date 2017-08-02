pub mod numeric {
    use std::iter::{Iterator,Sum};
    use std::ops::Sub;

    pub fn simpson_integral<F>(f: F, a: f32, b: f32, n: i32) -> f32
        where F: Fn(f32) -> f32
    {
        // Interval size; more means better accuracy
        let h:f32 = (b - a)/(n as f32);
        // Determine the correct coefficient
        let coef = |k:i32| -> f32 {
            match k {
                0 => 1.0,
                k if k == n => 1.0,
                k if k % 2 == 0 => 2.0,
                _ => 4.0,
            }
        };
        // Build a float32 range, which lacks syntax of its own
        let range = (1..n+1).map(|x| x as f32);
        // Incremental point calculation
        let term = |k:f32| -> f32 { coef(k as i32) * f(a + k * h)};
        // Sum our points over the integral's range and divide by a magic number
        h / 3.0 * sigma(term, range)
    }

    pub fn sigma<I, T, F>(func: F, iter: I) -> T
        where I: Iterator<Item=T>,
              T: Sum,
              F: FnMut(T) -> T
    {
        // Sum over the values returned by the Iterator, applying our function at each step
        return iter.map(func).sum()
    }

    // Approximate equality, within a given tolerance.
    pub fn approx_eq<T>(tol: T, a: T, b: T) -> bool
        where T: PartialEq + PartialOrd + Sub<Output=T>
    {
        let abs_diff = |x, y| if x >= y { x - y } else { y - x};
        abs_diff(a, b) <= tol
    }

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

    // Recursive version of Euclid's greatest common divisor algorithm.
    pub fn gcd(a: u32, b: u32) -> u32 {
        if let 0 = b {
            a
        } else {
            gcd(b, a % b)
        }
    }

    // Iterative version of Euclid's greatest commond divisor algorithm
    pub  fn gcd_iter(a: u32, b: u32) ->  u32 {
        let (mut x, mut y) = (a, b);
        while !(y == 0) {
            let r = x % y;
            x = y;
            y = r;
        }
        x
    }

    #[cfg(test)]
    mod tests {
        use math::numeric::*;

        #[test]
        fn test_simpson_integration() {
            let result = simpson_integral(|x| x.powi(3), 0.0, 1.0, 1000);
            let exp = 0.25;
            assert!(approx_eq(1.0e-6, result, exp),
                    "exp = {}, obs = {}", exp, result)
        }

        #[test]
        fn test_sigma() {
            // Closure types differ when defined _outside_ of a function argument
            // vs. on their own line, using `let`
            assert_eq!(55, sigma(|x| x, 1..11))
        }

        #[test]
        fn test_fibonacci()  {
            assert_eq!(fibonacci(0), 0);
            assert_eq!(fibonacci(1), 1);
            for i in 2..50 {
                assert_eq!(fibonacci(i), fibonacci(i-1) + fibonacci(i-2));
            }
        }

        #[test]
        fn gcd_test() {
            assert_eq!(gcd(206, 40), 2);
        }

        #[test]
        fn gcd_iter_test() {
            assert_eq!(gcd_iter(206, 40), 2);
        }
    }

}
