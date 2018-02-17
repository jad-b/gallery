def knapsack(S, K):
    """
    S ([int]): Size of items
    K (int): Size of knapsack
    """
    import collections
    Status = collections.namedtuple("Status", ["exists", "belongs"])
    # One row per item, one column per knapsack size
    p = [[Status(False, False)]*(K+1)] * len(S)

    for i in range(1, len(S)):
        for k in range(K+1):
            if p[i-1][k].exists:
                # Solution exists, but not using this item
                p[i][k] = Status(True, False)
            # Room for this item and there's a solution waiting if we use it
            elif k - S[i] >= 0 and p[i-1][k-S[i]].exists:
                p[i][k] = Status(True, True)
    return p
