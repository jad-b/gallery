using Base.Test
using DataStructs
# Means you *must* run it from above the DataStructs.jl directory. For now.
testdir = readdir("DataStructs.jl/test")
testfltr = x -> startswith(x, "test_")
@testset "$(f[6:end-3])" for f in Iterators.filter(testfltr, testdir)
    include(f)
end
