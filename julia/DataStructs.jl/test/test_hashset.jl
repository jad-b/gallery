@testset "constructors" begin
    @test typeof(HashSet()) == HashSet{Any}
    h = HashSet(1:10)
    @test typeof(h) == HashSet{Int64}
    @test length(h) == 10
end
@testset "basic data operations" begin
    h = HashSet{Int}()
    @test length(h) == 0
    for i=1:10
        push!(h, i)
        @test i in h
    end
    @test length(h) == 10
    for i=1:10
        delete!(h, i)
        @test !(i in h)
    end
    @test length(h) == 0
end
@testset "set operations" begin
    h = HashSet(1:10)
    @test union(h, 5:15) == HashSet(1:15)
    j = HashSet(5:15)
    @test union(h, j) == HashSet(1:15)
    @test diff(h, j) == HashSet([1,2,3,4,11,12,13,14,15])
    k = HashSet(2:8)
    @test intersect(h,j,k) == HashSet(5:8)
    @test subset(k,h)
    @test !subset(h,j)
end
