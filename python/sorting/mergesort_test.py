import random
import unittest


from .sorting import mergesort


class SortedAssert:

    def assertIsSorted(self, seq):
        prev = None
        for i, x in enumerate(seq):
            if prev is not None and x < prev:
                raise AssertionError("Sequence not sorted; {},{} @ {:d}"
                                     .format(prev, x, i))
            prev = x


class MergesortTestCase(unittest.TestCase, SortedAssert):

    def test_mergesort(self):
        values = list(range(0, 20))
        testcases = [
            values.copy(),
            random.shuffle(values.copy())
        ]
        for tc in testcases:
            with self.subTest(tc):
                obs = mergesort(tc)
                self.assertIsSorted(obs)
