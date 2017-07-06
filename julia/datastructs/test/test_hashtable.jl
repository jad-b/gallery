include("../src/hashtable.jl")

# Only compare KeyValues on their key attribute.
@test KeyValue{Int,Int}(1,2) == KeyValue{Int,Int}(1,2000)

let ht = ChainedHashTable{Int,String}(64)
    @test typeof(ht) == ChainedHashTable{Int,String}
    @test_throws KeyError isnull(search(ht, 4))

    # The hash table returned by insertion has been updated
    @test search(insert!(ht, 1, "1"), 1) == "1"
    # Inserting the same value under a new key doesn't affect lookups using the
    # old key
    @test search(insert!(ht, 2, "1"), 1) == search(ht, 1)

    # Deleting a non-existent value leaves an ummodified hash table
    # FIXME Test explodes on this one.
    #@test delete(ChainedHashTable{Int,String}(16), 1024) == ChainedHashTable{Int,String}(16)
    ## Deleting a value from the hash table leaves it in the same state as if the
    ## value was never inserted.
    #@test delete(insert(ht, 3, "3"), 3) == delete(ht, 3)
    ## Insertion & Deletion on different keys do not affect each other
    #@test delete(insert(ht, 4, "4"), 3) == insert(delete(ht, 3), 4, "4")
end

@testset "Iteration" begin
    let h = ChainedHashTable{Int,Char}(26)
        alpharange = Int('A'):Int('Z')
        for i=alpharange # Load the alphabet
            insert!(h, i, Char(i))
        end
        exp = [KeyValue{Int,Char}(i, Char(i)) for i in alpharange]
        obs = collect(h)
        @test exp == obs
    end
end
