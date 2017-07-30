using Base.Test
using DataStructs

"Alphabet"
ab = ['a':'z'; 'A':'Z']
"Generate a random string from the given alphabet and string length."
randstr(alphabet::Array{Char,1}=ab; len::Int=rand(8:12)) = string(rand(ab, len)...)

@testset "letter alphabet" begin
    a = LetterAlphabet()
    for c='a':'z'
        # identity function: c => i => c
        @test a[a[c]] == c
    end
    for c='A':'Z'
        i = a[c]
        @test (1 <= i <= 26)
        @test a[i] == lowercase(c)
    end
    @test maximum(a) == 26
end

@testset "basic data operations" begin
    a = LetterAlphabet()
    t = Trie{String}(maximum(a), a)
    @test length(t) == 0

    n = 5
    testwords = Array{String,1}(n)
    # Store values by key
    for i=1:n
        rstr = randstr(len=20)
        testwords[i] = rstr
        insert!(t, rstr, rstr)
        @test length(t) == i
    end
    # Search for nodes/values by key
    for word in testwords
        @test !isnull(DataStructs.findnode(t, word))
        rv = search(t, word)
        @test !isnull(rv.value) && get(rv) == word
        # Cutting a character from the word should (probably) make it return
        # nothing
        cut = rand(1:length(word))
        cutword = word[1:cut-1] * word[cut+1:end]
        @test isnull(DataStructs.findnode(t, cutword))
        @test isnull(search(t, cutword))
    end
    # Assert all keys are returned
    @test Set(keys(t)) == Set(map(lowercase, testwords))
    # Assert we can find keys given a prefix
    #for word in testwords
        #mid = length(word) >> 1
        #keys = keyswithprefix(t, word[1:mid])
        #@test word in keys
    #end
end
