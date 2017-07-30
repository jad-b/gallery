export Trie, LetterAlphabet
export keys, traverse

abstract type Alphabet end

"""
Maps ASCII letters to integers.
Uppercase characters are interpreted as lowercase.
"""
struct LetterAlphabet <: Alphabet
end

function Base.:getindex(a::LetterAlphabet, c::Char)
    if isupper(c)
        return Int(c) - 64
    elseif islower(c)
        return Int(c) - 96
    end
    error("$c is outside the allowed range [a-zA-Z]")
end

function Base.:getindex(a::LetterAlphabet, i::Int)
    if !(1 <= i <= 26)
        error("$i is outside the allowed range ([1,26])")
    end
    Char(i + 96)
end

Base.:maximum(a::LetterAlphabet) = 26

mutable struct Trie{V}
    """
    Value stored at this node. A non-null value indicates this string formed by
    following nodes along the path to this node is a valid key. Alternative
    implementations may just use a 'terminates' Boolean, or extend the child
    array to include a "special" slot for indicating a valid string.
    """
    value::Nullable{V}
    children::Array{Nullable{Trie{V}},1}
    "Number of values at or below this node."
    count::Int
    """
    Mapping of symbol <=> integer for the alphabet from which the string keys
    are drawn.
    """
    alphabet::Alphabet
end
function Trie{V}(size::Int, a::Alphabet) where V
    t = Trie(Nullable{V}(), Array{Nullable{Trie{V}},1}(size), 0, a)
    fill!(t.children, Nullable{Trie{V}}())
    t
end

"Trie length is the number of stored strings."
Base.:length(t::Trie{V}) where V = t.count

"Insert a single string into the Trie. Iterative."
function Base.:insert!(t::Trie{V}, key::String, val::V) where V
    node = t
    for k in key
        node.count += 1
        idx = node.alphabet[k]
        if isnull(node.children[idx])
            kid = Trie{V}(length(node.children), node.alphabet)
            node.children[idx] = Nullable(kid)
        end
        node = get(node.children[idx])
    end
    node.value = val
    t
end

"Return whether the key exists in the Trie or not."
Base.:in(t::Trie{V}, key::String) where V = !isnull(findnode(t, key))

"Search for the value stored with 'key'."
function Base.:search(t::Trie{V}, key::String) where V
    node = findnode(t, key)
    if isnull(node)
        return Nullable{V}()
    end
    get(node).value
end

"Return the node found by following the string 's'."
function findnode(t::Trie{V}, key::String) where V
    if key == "" # Return whatever we got.
        return Nullable(t)
    end
    # Find the next child to follow
    firstchar = key[1]
    childidx = t.alphabet[firstchar]
    child = t.children[childidx]
    if isnull(child) # That child doesn't exist; return null
        return Nullable{Trie{V}}()
    end
    # Recurse, using child
    findnode(get(child), key[2:end])
end

"""
Pre-order traversal of the Trie, where the parent is processed before the
children. The given function will be invoked on every non-null Trie node,
providing maximum flexibility through the use of anonymous functions.

Arguments:
- `pre::String`: Prefix built by including this Trie node.. If not starting
with the root node, the node should be obtained using `findnode` before calling
`traverse`.
"""
function traverse(t::Trie{V}, f::Function, pre::String="") where V
    f(t, pre) # Process this node
    for i=1:length(t.children)
        # map, with Nullables, handles the 'isnull' check for you.
        map(kid -> traverse(kid, f, pre*string(t.alphabet[i])), t.children[i])
    end
end

function Base.:keys(t::Trie{V}) where V
    keys = Array{String,1}(length(t))
    i = 1
    # Anonymous function to store valid keys
    coll = function (t::Trie{V}, pre::String) where V
        if !isnull(t.value)
            keys[i] = pre
            i += 1
        end
    end
    traverse(t, coll)
    keys
end

"List all keys that have a given prefix."
# keyswithprefix(T, p)
