import math


class EditDistance:

    def __init__(self, A, B):
        # Cost matrix for substitutions
        self.cost = [[math.inf] * len(B)] * len(A)
        # Changing from null string => null string is 0
        self.cost[0][0] = 0
        # Turning A_i => B_0 takes i deletions
        for i in range(1, len(A) + 1):
            self.cost[i][0] = i
        # Turning A_0 => B_j takes j insertions
        for j in range(1, len(B) + 1):
            self.cost[0][j] = j
        # Edits taken at each step, allowing for path reconstruction
        # self.edits = [[math.inf] * len(B)] * len(A)
        self.edit_distance(A, B)

    def edit_distance(self, A, B):
        """Finds the minimum edit distance between two strings."""
        for i in range(1, len(A) + 1):
            for j in range(1, len(B) + 1):
                # Cost of getting a match on A's previous character, allowing
                # us to delete A_i
                delete = self.cost[i-1][j]
                # Cost of matching against B_{j-1}. Appending b_j to A_i
                # results in a successful match between A_i v. B_j
                insert = self.cost[i][j-1]
                replace = self.cost[i-1][j-1]
                # Replacing is free if the characters match. Else, we have to
                # pay the cost of substitution
                if A[i] != B[j]:
                    replace += 1
                self.cost[i][j] = min(delete, insert, replace)
