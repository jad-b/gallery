import random


def quicksort(A):
    random.shuffle(A)  # Prevents worst-case performance
    _sort(A, 0, len(A)-1)


def _sort(A, lo, hi):
    if lo > hi:
        return
    j = _partition(A, lo, hi)
    _sort(A, lo, j-1)
    _sort(A, j+1, hi)


def _partition(A, lo, hi):
    pivot = select_pivot(A, lo, hi)
    i, j = lo, hi
    # Going out on a limb here
    while i < j:
        while A[i] < pivot:
            i += 1
        assert (A[i] >= pivot and i <= hi)
        while A[j] > pivot:
            j -= 1
        assert j >= lo
        assert (A[j] <= pivot and pivot <= A[i])
        A[i], A[j] = A[j], A[i]
        assert (A[i] <= pivot and pivot <= A[j])
    assert A[j] == pivot  # We've landed on our pivot element
    return j


def select_pivot(A, lo, hi):
    """Choose the pivot element.

    See:
        * Low/High
        * Median-of-medians
        * Random; obviates the need for shuffling at the start.
        * 3-way partitioning
    """
    return A[random.randint(lo, hi)]
