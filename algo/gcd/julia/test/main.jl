using Base.Test

include("../main.jl")

@testset "GCD Unit tests" begin
    @test gcd(206, 40) == 2
end
