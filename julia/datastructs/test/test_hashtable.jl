include("../src/hashtable.jl")

let ht = HashTable{Int,String}(1024)
    @test typeof(ht) == HashTable
    @test isnull(search(ht, 4))

    # The hash table returned by insertion has been updated
    @test search(insert(ht, 1, "1"), 1) == "1"
    # Inserting the same value under a new key doesn't affect lookups using the
    # old key
    @test search(insert(ht, 2, "1"), 1) == search(ht, 1)

    # Deleting a non-existent value leaves an ummodified hash table
    @test delete(HashTable{Int,String}(16), 15) == HashTable{Int,String}(16)
    # Deleting a value from the hash table leaves it in the same state as if the
    # value was never inserted.
    @test delete(insert(ht, 3, "3"), 3) == delete(ht, 3)
    # Insertion & Deletion on different keys do not affect each other
    @test delete(insert(ht, 4, "4"), 3) == insert(delete(ht, 3), 4, "4")
end
