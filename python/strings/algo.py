def ispermutation(s1, s2):
    """Determines if two strings are permutations of one another."""
    # Permutations must be equal in size
    if len(s1) != len(s2):
        return False
    # Count the values in string 1, and subtract for every time we see the same
    # value in string 2.
    ctr = {}
    for c in s1:
        ctr[c] += ctr.setdefault(c, 0) + 1
    for d in s2:
        if d in ctr:
            ctr[d] -= 1
            # If negative, s2 has more 'd' chars than s1
            if ctr[d] < 0:
                return False
    # If permutations, the counter should have nothing left inside of it.
    return len(ctr) == 0
