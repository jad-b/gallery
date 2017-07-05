struct KeyValue{K,V}
    key::K
    value::V
    KeyValue{K,V}(key::K, value::V) where {K,V} = new(key, value)
    # Incomplete Initialization
    KeyValue{K,V}(key::K) where {K,V} = new(key)
end

# Only compare for equality KeyValues on their key
function Base.:(==)(kv::KeyValue{K,V}, other::KeyValue{K,V}) where {K,V}
    kv.key == other.key
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

# Only compare for equality KeyValues on their key
function Base.:(==)(h::ChainedHashTable{K,V}, other::ChainedHashTable{K,V}) where {K,V}
    size = length(h.data)
    if size != length(other.data)
        return false
    end
    for i=1:size
        if !(isdef(h,i) && isdef(other,i)) || h.data[i] != other.data[i]
            return false
        end
    end
    true
end

# Test if the array slot has been defined
isdef(h::ChainedHashTable, i::Int) = isassigned(h.data, i)

function Base.:search(h::ChainedHashTable{K,V}, key::K) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        list::List = h.data[idx] # Get list at position
        kv::Nullable{KeyValue} = search(list, KeyValue{K,V}(key))
        if !isnull(kv)
            return get(kv).value
        end
    end
    throw(KeyError(key))
end

# Insert a (key,value) entry into the Hash Table.
function insert(h::ChainedHashTable{K,V}, key::K, value::V) where {K,V}
    idx = h.hash(key)
    if !isdef(h, idx)
        h.data[idx] = List()
    end
    # TODO Add upsert method to LinkedList
    insert(h.data[idx], KeyValue{K,V}(key, value))
    h
end

function delete(h::ChainedHashTable{K,V}, key::K) where {K,V}
    idx = h.hash(key)
    if isdef(h, idx)
        list::List = h.data[idx]
        delete(list, KeyValue{K,V}(key))
        # Alternatively, we could throw a KeyError on erroneous deletes, but
        # that breaks the idempotency
    end
    h
end
