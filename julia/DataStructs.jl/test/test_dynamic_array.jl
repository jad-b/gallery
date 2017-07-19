let da = DynamicArray{Int}()
    @testset "Empty" begin
        @test da.gf == 2.0
        @test length(da.arr) == 1
        @test typeof(da.arr) == Array{Int,1}
    end

    da[1] = 1
    @test da[1] == 1
    @test length(da.arr) == 1

    da[3] = 3
    @test da[3] == 3
    @test length(da.arr) == 4

    da[1000] = 1000
    @test da[1000] == 1000
    @test length(da.arr) == 1024
end

arr = collect(1:10)
d = DynamicArray{Int}(arr)
@test length(d) == length(arr)
