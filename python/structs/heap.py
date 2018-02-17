def heapify(q, i: int):
    "Move the item at index i into place by swapping with its children."
    smallest = i
    if q[smallest] < q[i*2]:  # left child
        smallest = i*2
    if q[smallest] < q[i*2+1]:  # right child
        smallest = i*2 + 1
    if smallest != i:  # Need to change
        q[i], q[smallest] = q[smallest], q[i]  # Swap
        heapify(q, smallest)
