#!/usr/bin/env python3
import sys


def longest_bit_flip(num):
    if ~num == 0:  # Test for all 1s
        return num.bit_length() * 8  # Number of bits in the int

    prev = curr = 0
    maximum = 1  # Can always make a run of 1
    while num != 0:
        if num & 0b1 == 1:
            curr += 1  # Keep tracking 1s
        else:
            maximum = max(curr + prev + 1, maximum)
            # Current run becomes previous run; current resets
            prev, curr = curr, 0
        # Artimetically shift the number right
        num >>= 1
    # End of number; check one last time
    return max(curr + prev + 1, maximum)


if __name__ == '__main__':
    num = int(sys.argv[1])
    best = longest_bit_flip(num)
    print("Longest run of 1s from 0->1 for {} is {}"
          .format(bin(num), str(best)))
