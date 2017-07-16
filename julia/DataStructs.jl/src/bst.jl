export BSTNodeType,BSTNode,EmptyBSTNode
export maximum,minimum,predecessor,successor

# A poor man's enum for records. I miss you, Rust.
abstract type BSTNodeType end

# Represents null values within the BST
struct EmptyBSTNode <: BSTNodeType end
const Empty = EmptyBSTNode()

# A node within a binary search tree
mutable struct BSTNode{T} <: BSTNodeType
    value::T
    left::BSTNodeType
    right::BSTNodeType
    parent::BSTNodeType
end
BSTNode{T}(x::T, parent::BSTNodeType=Empty) where T = BSTNode{T}(x, Empty,
                                                                 Empty, parent)
function Base.:length(node::BSTNode{T}) where T
    count = 0
    inorder(node, n -> count += 1)
    count
end

Base.:(==)(n::BSTNode{T}, other::BSTNode{T}) where T = n.value == other.value
Base.:<(n::BSTNode{T}, other::BSTNode{T}) where T = n.value < other.value

isleaf(n::BSTNode{T}) where T = n.left == Empty && n.right == Empty

Base.:search(node::EmptyBSTNode, ::T) where T = Nullable{T}()
function Base.:search(node::BSTNode{T}, x::T) where T
    n = search_node(node, x)
    if n == Empty
        Nullable{T}()
    else
        Nullable(n.value)
    end
end

search_node(node::EmptyBSTNode, ::T) where T = Empty
function search_node(node::BSTNode{T}, x::T) where T
    if x == node.value
        node
    elseif x < node.value
        search_node(node.left, x)
    else
        search_node(node.right, x)
    end
end

# Insert a new node into the BST. Equivalent values default to the left side.
# Return: The newly created node
Base.:insert!(n::EmptyBSTNode, x::T) where T =
    throw(ArgumentError("Can not insert! using an Empty node"))
Base.:insert!(node::BSTNode{T}, x::T) where T = insert_node(node, BSTNode{T}(x))
function insert_node(parent::BSTNodeType, node::BSTNode{T}) where T
    if node <= parent
        if parent.left == Empty
            parent.left, node.parent = node, parent
            node
        else
            insert_node(parent.left, node)
        end
    else
        if parent.right == Empty
            parent.right, node.parent = node, parent
            node
        else
            insert_node(parent.right, node)
        end
    end
end

# Remove the value 'x' from the BST, returning the node.
function Base.:delete!(node::BSTNode{T}, x::T) where T
    n::BSTNodeType = search_node(node, x)
    if n != Empty
        return delete_node(n)
    end
    Empty
end
function delete_node(node::BSTNode{T}) where T
    if node.left == Empty
        transplant(node, node.right)
    elseif node.right == Empty
        transplant(node, node.left)
    else # Two children
        y = minimum(node.right)
        if y != node.right
            transplant(y, y.right)
            y.right = node.right
            y.right.parent = y
        end
        transplant(node, y)
        y.left = node.left
        y.left.parent = y
    end
    node
end

# Transplant replaces one node with another by having the replaced node's
# parent point to the replacing node.
function transplant(old::BSTNodeType, new::BSTNodeType)
    if old.parent != Empty
        if old == old.parent.left
            old.parent.left = new
        elseif old == old.parent.right
            old.parent.right = new
        else
            throw(AssertionError("Drift between node's supposed and actual" *
                                 "parent"))
        end
    end
    if new != Empty
        new.parent = old.parent
    end
end

function predecessor(node::BSTNode{T}) where T
    if node.left != Empty
        maximum(node.left)
    else # Look upwards
        p = node.parent
        while p != Empty && p > node
            p = p.parent
        end
        p
    end
end

function successor(node::BSTNode{T}) where T
    if node.right != Empty
        minimum(node.right)
    else
        p = node.parent
        while p != Empty && p <= node
            p = p.parent
        end
        p
    end
end

Base.:minimum(node::EmptyBSTNode) =
    throw(ArgumentError("Can not use 'minimum' on an Empty node"))
function Base.:minimum(node::BSTNode{T}) where T
    n = node
    while n.left != Empty
        n = n.left
    end
    n
end

Base.:maximum(node::EmptyBSTNode) =
    throw(ArgumentError("Can not use 'maximum' on an Empty node"))
function Base.:maximum(node::BSTNode{T}) where T
    n = node
    while n.right != Empty
        n = n.right
    end
    n
end

function inorder(node::BSTNode{T}, callback) where T
    if node.left != Empty
        inorder(node.left, callback)
    end
    callback(node)
    if node.right != Empty
        inorder(node.right, callback)
    end
end
