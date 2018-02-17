def insertion_sort(a):
    # Start at 1; array of size 1 is already sorted
    for i in range(1, len(a)):
        # Assumption: a[0..i-1] is already sorted.
        # Starting at i and working down to 0, swap values as needed.
        # As soon as a[j] is greater than a[j-1], we're in place.
        j = i
        while j > 0 and a[j-1] > a[j]:
            a[j], a[j-1] = a[j-1], a[j]
            j -= 1
    return a
