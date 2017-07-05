struct KeyValue{K,V}
    key::K
    value::V
end

mutable struct ChainedHashTable{K,V}
    # An array of buckets
    data::Array{List,1}
    # Our custom hash function
    hash
end

ChainedHashTable{K,V}(n::Int, fn) where {K,V} = ChainedHashTable{K,V}(Array{List,1}(n), fn)
# Create a hash table of size n
ChainedHashTable{K,V}(n::Int) where {K,V} = ChainedHashTable{K,V}(n, x -> (hash(x)%Int) & (n-1) + 1)

# Test if the array slot has been defined
isdef(h::ChainedHashTable, i::Int) = isassigned(h.data, i)

function Base.:search(h::ChainedHashTable{K,V}, key::K) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        return (h.data[i]::KeyValue{K,V}).value::V
    end
    throw(KeyError(key))
end
