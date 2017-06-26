using Base.Test

include("../src/linked_list.jl")

let l = LinkedList.List()
    @testset "Empty list" begin
        @test isnull(l.head)
        @test isnull(l.tail)
        @test isnull(LinkedList.delete(l, 1))
        @test isnull(LinkedList.search(l, 1))
        @test isnull(LinkedList.minimum(l))
    end

    notnullandeq(thing, val) = !isnull(thing) && get(thing) == val
    LinkedList.insert(l, 1)
    @test get(l.head).elem == 1
    @test get(l.tail).elem == 1
    @test notnullandeq(LinkedList.search(l, 1), 1)

    LinkedList.insert(l, 2)
    @test get(l.head).elem == 2
    @test get(l.tail).elem == 1
    @test notnullandeq(LinkedList.search(l, 2), 2)

    delval = LinkedList.delete(l, 1)
    @test !isnull(delval) && get(delval).elem == 1


end
