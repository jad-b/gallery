# Multiway trees have a plethora of implementation strategies. If the maximum
# number of children per node is known, you can have a _k_-ary tree, where
# every node has _k_ child pointers. If you need more flexibility, you can
# use a list, or set aside a fixed number of pointers and use a list to handle
# excess children.
#
# Is it a _search_ tree? If so, then you'll need to maintain an order amongst
# your children. Ternary (3) search trees have three children per node,
# matching the operations <,=,>, respectively. More than that? Try a B-tree,
# keeping m/2 to m children per node.
#
# Any multiway tree can be made into a binary tree by use of the left-child
# right-sibling representation. Left-most children are kept in the left child
# pointer. All other children are chained together by means of `right-sibling`
# pointers. Thus, the parent only points to the left-most child, and accesses
# the other children by walking the list of siblings. Ignorning the "sibling"
# context, you return to having nodes with a left & right pointer: a binary
# tree.
#
# K-ary tree (unordered)
# B-tree (search)
# Left-Child Right-Sibling

