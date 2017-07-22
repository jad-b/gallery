using Base.eltype
export HashSet, union, intersection, subset

mutable struct HashSet{T}
    dict::ChainedHashTable{T,Void}

    HashSet{T}() where T = new(ChainedHashTable{T,Void}())
    HashSet{T}(n::Int) where T = new(ChainedHashTable{T,Void}(n))
    # Make an empty set, then push values into it
    HashSet{T}(iter) where T = union!(new(ChainedHashTable{T,Void}(length(iter))), iter)
end
# Un-typed constructors with type inference
HashSet() = HashSet{Any}()
HashSet(iter) = HashSet{eltype(iter)}(iter)

function Base.:(==)(s::HashSet{T}, t::HashSet{T}) where T
    if length(s) != length(t)
        return false
    end
    mapreduce(x->x in t, +, s) == length(s)
end
Base.:length(s::HashSet{T}) where T = length(s.dict)
Base.:push!(s::HashSet{T}, x::T) where T = insert!(s.dict, x, nothing)
Base.:delete!(s::HashSet{T}, x::T) where T = delete!(s.dict, x)
in(x::T, s::HashSet{T}) where T = x in s.dict
Base.:eltype(::Type{HashSet{T}}) where T = T
sizehint!(s::HashSet{T}, newsz) where T = (s.dict = sizehint!(s.dict, newsz); s)

# Lean on the underlying Dictionary's representation
Base.start(iter::HashSet{T}) where T = start(iter.dict)
Base.done(iter::HashSet{T}, state) where T = done(iter.dict, state)
function Base.next(iter::HashSet{T}, state) where T
    kv::KeyValue{T,Void}, state = next(iter.dict, state)
    kv.key, state # Extract just the key
end



# union
Base.union!(s::HashSet{T}, xs) where T = (for x=xs; push!(s,x); end; s)
Base.union!(s::HashSet{T}, xs::AbstractArray) where T =
    (sizehint!(s,length(xs));for x=xs; push!(s,x); end; s)
# Non-modifying union
function Base.:union(s::HashSet{T}, sets::HashSet{T}...) where T
    # We know we'll need _at least_ as much space as the larger of the two sets.
    # Worst case (space-wise), they're completely unique, so we'll need to grow,
    # which is handled by the hash table. Best case, the smaller is a subset of
    # the larger, so we don't need to allocate any more space.
    u = HashSet{T}(max(length(s), length(sets)))
    union!(u, s)
    for t in sets
        union!(u, t)
    end
    u
end
function Base.:union(s::HashSet{T}, xs::AbstractArray) where T
    u = HashSet{T}(max(length(s), length(xs)))
    union!(u, s)
    for y in xs
        push!(u, y)
    end
    u
end
# Difference between two sets
# Returns: A new HashSet containing the difference.
function Base.:diff(s::HashSet{T}, t::HashSet{T}) where T
    u = HashSet{T}(max(length(s), length(t)))
    for x in s
        if !(x in t)
            push!(u, x)
        end
    end
    for y in t
        if !(y in s)
            push!(u, y)
        end
    end
    u
end

# Intersection between two or more sets.
function Base.:intersect(s::HashSet{T}, sets::HashSet{T}...) where T
    src = s # Starting set
    start = 1 # First index into sets
    # Accumulator set
    u = HashSet{T}(max(length(s), map(length, sets)...))
    while start < length(sets)
        union!(u, # accumulate values the pass the filter
               Iterators.filter(
                   # Only values in all other sets
                   x->all( s->x in s, sets[start:end]), #
                   src)) # Source of values to compare with
        src = sets[start]
        start += 1
    end
    u
end

# Checks if s is a subset of t, in typical fashion for ⊂.
subset(s::HashSet{T}, t::HashSet{T}) where T = all(x->x in t, s)
