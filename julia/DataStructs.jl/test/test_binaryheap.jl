@test_throws ArgumentError BinaryHeap{Int}(0, <)

# Inner constructor
h = BinaryHeap{Int}(collect(10:-1:1), <, 10)
promote!(h, 10) # Move 1 to the first index
@test h.data == [1,10,8,7,9,5,4,3,2,6]
h = BinaryHeap{Int}(collect(10:-1:1), <, 10)
demote!(h, 1) # Move 10 down the heap
@test h.data == [8,9,4,7,6,5,10,3,2,1]

# Build a Heap from repeated insertions
h = BinaryHeap{Int}(10, <)
for i=shuffle(1:10)
    insert!(h, i)
end
@test length(h) == 10
for i=1:10
    @test extract!(h) == i
end

# Construct from an existing array
h = BinaryHeap{Int}(shuffle(1:10), <)
@test length(h) == 10
for i=1:10
    @test extract!(h) == i
end

# Tests using '>'
@test heapsort!(shuffle(1:10)) == collect(1:10)

# Use a DynamicArray beneath the covers
d = DynamicArray(shuffle(1:10))
h = BinaryHeap{Int}(d, <)
@test length(h.data) == 10
for i=shuffle(11:20)
    insert!(h, i)
end
@test length(h) == 20
