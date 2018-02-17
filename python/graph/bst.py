class BST:

    def __init__(self, value, parent, left=None, right=None):
        self.value = value
        self.parent = parent
        self.left = left
        self.right = right

    def transplant(self, new):
        if self.parent is not None:  # Not the root
            if self == self.parent.left:
                self.parent.left = new
            else:
                self.parent.right = new
        if new is not None:
            new.parent = self.parent

    def delete(self, x):
        node = search_node(self, x)
        if node is None:
            return self
        if node.left is None:  # Right child or no children
            node.transplant(node.right)
        elif node.right is None:  # Left child
            node.transplant(node.left)
        else:  # Both children
            pred = node.left.maximum()  # Find the predecessor
            if pred != node.left:
                pred.transplant(pred.left)  # Backfill pred with its left child
                # Assign node-to-replace's left child under the pred
                pred.left = node.left
                node.left.parent = pred
            node.transplant(pred)
            pred.right = node.right
            pred.right.parent = pred
            return self
