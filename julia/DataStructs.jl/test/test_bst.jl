primes=[2,3,5,7,11,13,17,19,23,29,31]
mid = (length(primes) >> 1) + 1
const T = Integer
root = BSTNode{T}(primes[mid])
@test DataStructs.isleaf(root)
@test BSTNode{T}(primes[mid]) == root
@test BSTNode{T}(primes[mid-1]) < root
@test length(root) == 1
@test minimum(root) == root
@test maximum(root) == root
@test predecessor(root) == DataStructs.Empty
@test successor(root) == DataStructs.Empty

@test insert!(root,5) === root.left
@test insert!(root,23) === root.right
@test insert!(root,2) === root.left.left
@test insert!(root,31) === root.right.right
@test length(root) == 5
@test minimum(root).value == 2
@test maximum(root).value == 31
@test predecessor(root.left).value == 2
@test successor(root.left).value == 13
@test predecessor(root.right).value == 13
@test successor(root.right).value == 31
# We have now formed an upside-down 'V', fyi.
for v in [3,7,11,17,19,29]  # Build the rest of the BST
    insert!(root,v)
end
@test length(root) == 11
#         13
#    5          23
#  2   7    17      31
#   3   11    19  29
@test minimum(root).value == primes[1]
@test maximum(root).value == primes[end]

@test delete!(root, 1) == DataStructs.Empty
@test delete!(root, 19).value == 19 # Delete leaf
@test delete!(root, 7).value == 7 # Delete parent-of-one
@test delete!(root, 23).value == 23 # Delete parent-of-two
#         13
#    5          29
#  2   11    17    31
#   3
@test root.right.value == 29
@test length(root) == 8

@test insert!(root, 16) == root.right.left.left
@test insert!(root, 15) == root.right.left.left.left
#         13
#    5          29
#  2   11    17    31
#   3      16
#         15
rrlll = root.right.left.left.left
# Verify we recurse upwards correctly
@test predecessor(rrlll) == root
@test successor(root.left.left.right).value == 5
