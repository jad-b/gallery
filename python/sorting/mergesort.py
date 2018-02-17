#!/usr/bin/env python3


def mergesort(A):
    aux = [None] * len(A)  # Allocate space once
    _sort(A, aux, 0, len(A)-1)
    return A  # Unnecessary; mergesort is in-place


def _sort(A, aux, lo, hi):
    """Recursively "sort" both halves, then merge them together."""
    if lo >= hi:
        return
    # Alt: lo + (hi-lo)/2. Both avoid overflow.
    mid = (lo + hi) >> 1
    _sort(A, aux, lo, mid)
    _sort(A, aux, mid+1, hi)
    _merge(A, aux, lo, mid, hi)


def _merge(A, aux, lo, mid, hi):
    # Temporary buffer for the sections we're about to merge.
    aux[lo:hi+1] = A[lo:hi+1]

    i, j = lo, mid+1
    for k in range(lo, hi+1):
        if i > mid:  # Done with left; take remaining from right
            A[k] = aux[j]
            j += 1
        elif j > hi:  # Done with right; take remaining from left
            assert i <= mid
            A[k] = aux[i]
            i += 1
        elif aux[i] < aux[j]:  # Left smaller than right
            assert (i <= mid and j <= hi)
            A[k] = aux[i]
            i += 1
        else:  # Right smaller than left
            assert (i <= mid and j <= hi and aux[j] <= aux[i])
            A[k] = aux[j]
            j += 1
