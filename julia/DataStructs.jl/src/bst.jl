export maximum,minimum,predecessor,successor
export BinaryNodeType,BinaryNode,EmptyBinaryNode

# A poor man's enum for records. I miss you, Rust.
abstract type BinaryNodeType end

# Represents null values within the Binary
struct EmptyBinaryNode <: BinaryNodeType end
const Empty = EmptyBinaryNode()

# A node within a binary search tree
mutable struct BinaryNode{T} <: BinaryNodeType
    value::T
    left::BinaryNodeType
    right::BinaryNodeType
    parent::BinaryNodeType
end
BinaryNode{T}(x::T, parent::BinaryNodeType=Empty) where T =
    BinaryNode{T}(x, Empty, Empty, parent)

function Base.:length(node::BinaryNode{T}) where T
    count = 0
    inorder(node, n -> count += 1)
    count
end

Base.:(==)(n::BinaryNode{T}, other::BinaryNode{T}) where T = n.value == other.value
Base.:<(n::BinaryNode{T}, other::BinaryNode{T}) where T = n.value < other.value

isleaf(n::BinaryNode{T}) where T = n.left == Empty && n.right == Empty

Base.:search(node::EmptyBinaryNode, ::T) where T = Nullable{T}()
function Base.:search(node::BinaryNode{T}, x::T) where T
    n = search_node(node, x)
    if n == Empty
        Nullable{T}()
    else
        Nullable(n.value)
    end
end

search_node(node::EmptyBinaryNode, ::T) where T = Empty
function search_node(node::BinaryNode{T}, x::T) where T
    if x == node.value
        node
    elseif x < node.value
        search_node(node.left, x)
    else
        search_node(node.right, x)
    end
end

# Insert a new node into the Binary. Equivalent values default to the left side.
# Return: The newly created node
Base.:insert!(n::EmptyBinaryNode, x::T) where T =
    throw(ArgumentError("Can not insert! using an Empty node"))
Base.:insert!(node::BinaryNode{T}, x::T) where T = insert_node(node, BinaryNode{T}(x))
function insert_node(parent::BinaryNodeType, node::BinaryNode{T}) where T
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

# Remove the value 'x' from the Binary, returning the node.
function Base.:delete!(node::BinaryNode{T}, x::T) where T
    n::BinaryNodeType = search_node(node, x)
    if n != Empty
        return delete_node(n)
    end
    Empty
end
function delete_node(node::BinaryNode{T}) where T
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
function transplant(old::BinaryNodeType, new::BinaryNodeType)
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

function predecessor(node::BinaryNode{T}) where T
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

function successor(node::BinaryNode{T}) where T
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

Base.:minimum(node::EmptyBinaryNode) =
    throw(ArgumentError("Can not use 'minimum' on an Empty node"))
function Base.:minimum(node::BinaryNode{T}) where T
    n = node
    while n.left != Empty
        n = n.left
    end
    n
end

Base.:maximum(node::EmptyBinaryNode) =
    throw(ArgumentError("Can not use 'maximum' on an Empty node"))
function Base.:maximum(node::BinaryNode{T}) where T
    n = node
    while n.right != Empty
        n = n.right
    end
    n
end

function inorder(node::BinaryNode{T}, callback) where T
    if node.left != Empty
        inorder(node.left, callback)
    end
    callback(node)
    if node.right != Empty
        inorder(node.right, callback)
    end
end
