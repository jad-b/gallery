export DynamicArray, grow!

# DynamicArray is a resizable one-dimensional array
mutable struct DynamicArray{T} <: AbstractArray{T,1}
    # Underlying array
    arr::Array{T,1}
    # Growth Factor
    gf::Float64
    # TODO Track the largest filled index
    # maxidx::Int

    function DynamicArray{T}(arr::Array{T,1}, gf::Float64=2.0) where T
        if gf < 1.0
            error("Growth Factor must be greater than 1")
        end
        new(arr, gf)
    end
end
DynamicArray{T}(gf::Float64) where T = DynamicArray{T}(Array{T,1}(1), gf)
DynamicArray{T}() where T = DynamicArray{T}(2.0)

Base.size(DA::DynamicArray{T}) where T = size(DA.arr)
Base.IndexStyle(::Type{<:DynamicArray{T}}) where T = IndexLinear()
Base.getindex(DA::DynamicArray{T}, i::Int) where T = DA.arr[i]

function Base.:setindex!(DA::DynamicArray{T}, v, i::Int) where T
    while i > length(DA.arr)
        # Inefficient, but correct
        grow!(DA)
    end
    assert(length(DA.arr) >= length(DA.arr))
    DA.arr[i] = v
    v
end

function Base.:push!(DA::DynamicArray{T}, i::Int) where T
    while i > length(DA.arr)
        # Inefficient, but correct
        grow!(DA)
    end
    assert(length(DA.arr) >= length(DA.arr))
    DA.arr[i] = v
end

# Expand the underlying array to include stuff.
function grow!(DA::DynamicArray{T}) where T
    newsize = convert(Int, ceil(DA.gf * length(DA.arr)))
    arr = Array{T,1}(newsize)
    for i in 1:length(DA.arr)
        arr[i] = DA.arr[i]
    end
    DA.arr = arr
end
