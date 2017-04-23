# Greatest Common Divisor
Finding greatest common divisors (GCDs) using Euclid's algorithm.

The algorithm works off of the observation that the GCD of two numbers is the
same as the GCD of the smaller number and the remainder of the larger divider by
the smaller. Letting a=the larger value, b=the smaller value, and r=the remainder of
a / b, we have GCD(a,b) = GCD(b,r). By repeating this substitution until b=0,
our a will then equal the GCD.

```
gcd(a,b)
    if b == 0
        return a
    return gcd(b, a % b)
```
