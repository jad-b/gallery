def delete(l, x):
    node = search(l, x)
    if node is None:
        return l
    if node.prev is None:  # Head
        l.head = node.next
    else:
        l.prev.next = node.next
    if node.next is None:  # Tail
        l.tail = node.prev
    else:
        l.next.prev = node.prev
    l.count -= 1
    return l
