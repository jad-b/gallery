export BinaryHeap, promote!, demote!, extract!, heapsort!

# A binary heap with a customizable comparator function.
mutable struct BinaryHeap{T}
    # Data storage. Consider using a dynamic array if the heap size isn't known
    # at runtime.
    data::AbstractArray{T,1}
    # Comparison function
    cmp
    # Number of elements in the heap
    size::Int

    function BinaryHeap{T}(arr::AbstractArray{T,1}, cmp, size::Int) where T
        if length(arr) < 1
            throw(ArgumentError("Can not allocate Binary Heap of size zero"))
        end
        new(arr, cmp, size == 0 ? 0 : size)
    end
end
BinaryHeap{T}(size::Int, cmp) where T = BinaryHeap{T}(Array{T,1}(size), cmp, 0)
# Also known as "build-max-heap" [CLRS09].
function BinaryHeap{T}(arr::AbstractArray{T,1}, cmp) where T
    h = BinaryHeap{T}(arr, cmp, length(arr))
    mid = h.size >> 1
    for i=mid:-1:1
        demote!(h, i)
    end
    h
end

Base.:length(h::BinaryHeap{T}) where T = h.size
Base.:getindex(h::BinaryHeap{T}, i::Int) where T = h.data[i]
Base.:setindex!(h::BinaryHeap{T}, v, i::Int) where T = h.data[i] = v

# Return the given index if out-of-bounds. Chosen because left/right indexing
left(i::Int) =  i<<1
right(i::Int) = (i<<1)+1
parent(i::Int) = i < 1 ? error("root has no parent") : i >> 1

# Throws:
#   ErrorException: If out of space to insert.
function Base.:insert!(h::BinaryHeap{T}, x::T) where T
    try
        h.data[h.size+1] = x
    catch x
        if isa(x, BoundsError)
            error("out of space")
        else rethrow(x) end
    end
    h.size += 1
    promote!(h, h.size)
end

# Extracts the least "extreme" value of the heap, the root. If a min-heap, this
# will be the smallest value in the heap; for a max-heap, vice-versa.
function extract!(h::BinaryHeap{T}) where T
    if h.size < 1
        error("no values in heap")
    end
    root, h[1] = h[1], h[h.size]
    h.size -= 1
    demote!(h, 1)
    root
end

function promote!(h::BinaryHeap{T}, i::Int) where T
    if i == 1
        return
    end
    p = parent(i)
    if h.cmp(h[i], h[p])
        h[i], h[p] = h[p], h[i] # Swap
        promote!(h, p)
    end
end

# Also known as "heapify"
function demote!(h::BinaryHeap{T}, i::Int) where T
    if i == h.size
        return
    end
    idx = i
    # Find best child to replace parent, if exists
    child = left(i)
    for j=0:1
        c = child+j
        if c <= h.size && h.cmp(h[c], h[idx])
            idx = c
        end
    end
    if idx != i # Child should replace parent?
        h[i], h[idx] = h[idx], h[i] # Swap
        demote!(h, idx)
    end
end

# Repeatedly move the root (max) value to the end of the array, decrease the
# size of the heap so as not to move it again, and re-heapify the new root.
#
# Returns the same array, now sorted.
function heapsort!(arr::AbstractArray{T,1}) where T
    h = BinaryHeap{T}(arr, >) # Triggers a max-heapifying
    for i=h.size:-1:2
        h[1], h[h.size] = h[h.size], h[1]
        h.size -= 1
        demote!(h, 1)
    end
    h.data
end
