include("./linked_list.jl")

mutable struct ChainedHashTable{K,V}
    # An array of buckets
    data::Array{List,1}
    # Our custom hash function
    hash::Function
end

struct KeyValue{K,V}
    key::K
    value::V
end

HashTable{K,V}(n::Unsigned, fn::Function) where {K,V} = HashTable(Array{List,1}(n), fn)
# Create a hash table of size n
HashTable{K,V}(n::Unsigned) where {K,V} = HashTable(n, x -> (hash(x)%Int) & (n-1) + 1)

isdef(h::ChainedHashTable, i::Int) = isassigned(h.data, i)

function search(h::ChainedHashTable{K,V}, key) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        return (h.data[i]::KeyValue).value::V
    end
    throw(KeyError(key))
end
