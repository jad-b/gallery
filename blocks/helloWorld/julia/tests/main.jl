using Base.Test

include("../main.jl")

@testset "Hello World tests" begin
    @test hello() == "Hello, world!"
    @test hello("jdb") == "Hello, jdb!"
end
