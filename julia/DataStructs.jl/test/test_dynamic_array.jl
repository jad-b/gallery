@testset "basic data operations" begin
    da = DynamicArray{Int}()
    @test da.gf == 2.0
    @test da.maxidx == 0 && length(da) == da.maxidx
    @test da.maxcapacity == typemax(Int)
    @test typeof(da.data) == Array{Int,1}

    da[1] = 1
    @test da[1] == 1
    # The array must be grown to the desired length
    @test_throws BoundsError (da[10] = 10)
    # Insertions to the "end" of the array are allowed
    insert!(da, 2)
    @test length(da) == 2
    # The length reflects the stored elements, not the size of the underlying
    # array
    grow!(grow!(da))
    @test length(da) == 2
    @test length(da.data) == 8
end

@testset "capacity" begin
    da = DynamicArray{Int}(10, 1.5, 30)
    @test length(da) == 0
    @test da.gf == 1.5
    @test da.maxidx == 0
    @test da.maxcapacity == 30

    grow!(da)
    @test length(da.data) == 15 # 1.5 growth factor
    grow!(da)
    @test length(da.data) == 23 # Ceiling
    grow!(da)
    @test length(da.data) == 30 # At capacity
    @test_throws ErrorException grow!(da)
end
