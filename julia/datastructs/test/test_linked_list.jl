using Base.Test

include("../src/linked_list.jl")

let l = List()
    @testset "Empty list" begin
        @test isnull(l.head)
        @test isnull(l.tail)
        @test isnull(delete(l, 1))
        @test isnull(search(l, 1))
        @test isnull(minimum(l))
    end

    notnullandeq(thing, val) = !isnull(thing) && get(thing) == val
    @testset "Creation, deletion, and search" begin
        insert(l, 1)
        @test get(l.head).elem == 1
        @test get(l.tail).elem == 1
        @test notnullandeq(search(l, 1), 1)
        @test notnullandeq(minimum(l), 1)

        insert(l, 2)
        @test get(l.head).elem == 2
        @test get(l.tail).elem == 1
        @test notnullandeq(search(l, 2), 2)
        @test notnullandeq(minimum(l), 1)

        delval = delete(l, 1)
        @test !isnull(delval) && get(delval).elem == 1
        @test notnullandeq(minimum(l), 2)
    end

    #@testset "More values" begin
        #for i=10:-1:3
            #insert(l, i)
        #end
        #@test notnullandeq(minimum(l), 2)

        #while !isnull(l.head)
            #delete(l, get(l.head).elem)
        #end
    #end

    #@testset "Empty after draining" begin
        #@test isnull(l.head)
        #@test isnull(l.tail)
        #@test isnull(delete(l, 1))
        #@test isnull(search(l, 1))
        #@test isnull(minimum(l))
    #end
end
