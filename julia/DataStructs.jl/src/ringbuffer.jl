export RingBuffer, isfull, write!

mutable struct RingBuffer{T}
    buf::Array{T,1}
    wptr::Integer
    rptr::Integer
    size::Integer
end
function RingBuffer{T}(size::Integer) where T
    if size <= 0
        throw(ArgumentError("Size must be greater than 0"))
    end
    RingBuffer(Array{T,1}(size), 1, 1, 0)
end

function Base.:isempty(b::RingBuffer{T}) where T
    if b.size == 0
        @assert b.rptr == b.wptr
        true
    else
        false
    end
end

function isfull(b::RingBuffer{T}) where T
    if b.size == length(b.buf)
        @assert b.wptr == b.rptr
        true
    else
        false
    end
end

modinc(b::RingBuffer{T}, x::Integer) where T = x % length(b.buf) + 1

function Base.:read!(b::RingBuffer{T}) where T
    if isempty(b)
        error("no values available to read")
    end
    rv = b.buf[b.rptr]
    b.rptr = modinc(b, b.rptr)
    b.size -= 1
    rv
end

function write!(b::RingBuffer{T}, x::T) where T
    if isfull(b) # Going to overwite
        b.rptr = modinc(b, b.rptr) # Advance read pointer to next oldest slot
    else
        b.size += 1
    end
    b.buf[b.wptr] = x # Write
    b.wptr = modinc(b, b.wptr) # Advance write pointer
    b
end
