using Base.Test

include("../main.jl")

@testset "File I/O tests" begin
    @test readInFile("../testdata/hi.txt") == "hi"
end
