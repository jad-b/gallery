using Base.Test

include("../main.jl")

@testset "GCD Unit tests" begin
    @test euclids(206, 40) == 2
end

@testset "Fibonacci unit tests" begin
    @testset "Base cases" begin
        @test fib(0) == 0
        @test fib(1) == 1
    end
    @testset "n=$i" for i  in 2:50
        @test fib(i) == +(fib(i-1), fib(i-2))
    end
end
