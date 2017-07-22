@testset "KeyValue" begin
    # Only compare KeyValues on their key attribute.
    @test DataStructs.KeyValue{Int,Int}(1,2) == DataStructs.KeyValue{Int,Int}(1,2000)
end

@testset "Dictionary Methods" begin
    h = ChainedHashTable{Int,String}(64)
    @test typeof(h) == ChainedHashTable{Int,String}
    @test_throws KeyError isnull(h[4])

    # The hash table returned by insertion has been updated
    @test insert!(h, 1, "1")[1] == "1"
    @test loadfactor(h) == 1/64
    # Inserting the same value under a new key doesn't affect lookups using the
    # old key
    @test insert!(h, 2, "1")[1] == h[1]
    # Values can be updated beneath their key
    @test insert!(h,2,"2")[2] == "2"
    @test loadfactor(h) == 1/32

    # Deleting a non-existent value leaves an ummodified hash table
    @test delete!(ChainedHashTable{Int,String}(16), 1024) == ChainedHashTable{Int,String}(16)
    # Deleting a value from the hash table leaves it in the same state as if the
    # value was never inserted.
    count = length(h)
    @test length(delete!(insert!(h, 3, "3"), 3)) == count
end

@testset "Iteration" begin
    @testset "Empty" begin
        h = ChainedHashTable{Int,Char}(26)
        @test DataStructs.next_node(h, 1)[1] == 27
    end
    @testset "1-Element" begin
        h = ChainedHashTable{Int,Char}(26)
        insert!(h,1,'A')
        state1 = DataStructs.next_node(h,1)
        idx = h.hash(1)
        @test state1[1] == idx
        @test !isnull(state1[2])
        state2 = DataStructs.next_node(h,idx+1)
        @test state2[1] == 27
        @test isnull(state2[2])
    end
    @testset "3-Element" begin
        h = ChainedHashTable{Int,Char}(26)
        for c='A':'C'
            insert!(h,Int(c),c)
        end
        idx, node = DataStructs.next_node(h,1)
        count = 0
        while idx <= length(h.data)
            # Invariant: List will be non-null while index < data length
            list::List = h.data[idx]
            count += list.count
            idx, node = DataStructs.next_node(h,idx+1)
        end
        @test count == h.count
    end
    @testset "3-element" begin
        h = ChainedHashTable{Int,Char}(26)
        for i='A':'C'
            insert!(h, Int(i), i)
        end
        @test h.count == 3
        state = start(h)
        idx, node = state
        h_len = length(h.data)
        @test idx <= h_len
        @test !isnull(node)
        count = 0
        while !done(h, state)
            # Check current state is fine
            idx, node = state
            #println("idx=$idx, node=$node")
            @test idx <= h_len && !isnull(node)
            # Assert
            (val, state) = next(h, state)
            @test isa(val, DataStructs.KeyValue{Int,Char})
        end
        @test state[1] > h_len
    end
    @testset "Alphabet" begin
        h = ChainedHashTable{Int,Char}(26)
        alpharange = 'A':'Z'
        for c=alpharange # Load the alphabet
            insert!(h, Int(c), c)
        end
        @test h.count == 26
        keys = Set([Int(c) for c in alpharange])
        vals = Set(collect(alpharange))
        obs = collect(h)
        @test keys == Set(map(x -> x.key, obs))
        @test vals == Set(map(x -> x.value, obs))
    end
end

@testset "Volume" begin
    n = 1024
    h = ChainedHashTable{Int,String}(256)
    for i=1:n
        insert!(h, i, string(i))
    end
    @test h.count == n
    @test loadfactor(h) == 4.0
    # All values accounted for?
    @test Set(1:n) == Set(map(x->x.key, collect(h)))
end

@testset "Resizing" begin
    h = ChainedHashTable{Int,String}(2)
    n = 1<<8 # 256
    for i=1:n
        insert!(h, i, string(i))
    end
    @test length(h) == n
    for j=1:8 #; 256x growth
        lf, oldsize, oldcount = loadfactor(h), length(h.data), length(h)
        resize!(h)
        @test length(h.data) == 2 * oldsize
        @test length(h) == oldcount
        @test loadfactor(h) == lf/2
    end
    # All keys accounted for?
    @test Set(1:n) == Set(map(x->x.key, collect(h)))
end
