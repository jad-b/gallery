export DynamicArray, grow!

# DynamicArray is a resizable one-dimensional array
mutable struct DynamicArray{T} <: AbstractArray{T,1}
    # Underlying array
    data::Array{T,1}
    # Growth Factor
    gf::Float64
    # Highest index value seen in the array
    maxidx::Int
    # Upper limit to growth. It defaults to the maximum allowed value.
    maxcapacity::Int

    function DynamicArray{T}(arr::Array{T,1},
                             gf::Float64,
                             maxidx::Int,
                             cap::Int) where T
        if gf <= 1.0
            error("Growth Factor must be greater than 1")
        end
        new(arr, gf, maxidx, cap)
    end
end
function DynamicArray{T}(size::Int=1,
                         gf::Float64=2.0,
                         cap::Int=typemax(Int)) where T
    DynamicArray{T}(Array{T,1}(size), gf, 0, cap)
end
function DynamicArray(arr::Array{T,1},
                      gf::Float64=2.0,
                      cap::Int=typemax(Int)) where T
    DynamicArray{eltype(arr)}(arr, gf, length(arr), cap)
end

# Abstract Array
Base.:size(a::DynamicArray{T}) where T = size(a.data)
Base.:length(a::DynamicArray{T}) where T = a.maxidx
Base.:IndexStyle(::Type{<:DynamicArray{T}}) where T = IndexLinear()
Base.:getindex(a::DynamicArray{T}, i::Int) where T = a.data[i]
"""
Set the value at index `i` in the array. To preserve the abstraction of an
elastically growable array, the upper limit of `i` is capped at
the the maximum index seen, regardless of the array's actual size.

Inserting new values should be accomplished by the use of the two-argument form
of `insert!(a,x)`.
"""
function Base.:setindex!(a::DynamicArray{T}, v, i::Int) where T
    if i > a.maxidx
        throw(BoundsError(a, i))
    end
    a.data[i]=v
    v
end

# List ADT

"Add a value into the dynamic array, resizing the array if necessary."
function Base.:insert!(a::DynamicArray{T}, x::T) where T
    if a.maxidx == length(a.data)
        try
            grow!(a)
        catch x
            println(x)
            throw(BoundsError(a, a.maxidx+1))
        end
    end
    @assert a.maxidx < length(a.data)
    a.maxidx += 1
    a.data[a.maxidx] = x
    a
end

"Find the value using a linear search."
function Base.:getindex(a::DynamicArray{T}, x::T) where T
    for y in a.data
        if x == y
            return y
        end
    end
    error("no value $x was found")
end

"""
Expand the underlying array to include more stuff.

If the array is already at its max capacity, then an error will be raised to
indicate this to the user.
"""
function grow!(a::DynamicArray{T}) where T
    if length(a.data) == a.maxcapacity
        error("can not grow array; at capacity ($(a.maxcapacity))")
    end
    newsize = min(a.maxcapacity, convert(Int, ceil(a.gf * length(a.data))))
    arr = Array{T,1}(newsize)
    for i in 1:length(a.data)
        arr[i] = a.data[i]
    end
    a.data = arr
    a
end
