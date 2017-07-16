@test_throws ArgumentError RingBuffer{Float64}(0)

let b = RingBuffer{Float64}(3)
    @test DataStructs.modinc(b, 1) == 2
    @test DataStructs.modinc(b, 2) == 3
    @test DataStructs.modinc(b, 3) == 1
end

let b = RingBuffer{Float64}(1)
    @testset "Empty" begin
        @test isempty(b)
        @test_throws ErrorException read!(b)
    end
    @testset "Normal" begin
        write!(b, 1.)
        @test b.wptr == 1
        @test b.rptr == 1
        @test b.size == 1
        @test isfull(b)
        @test read!(b) == 1.
        @test isempty(b)
    end
    @testset "Overwrite" begin
        write!(b, 2.)
        write!(b, 3.)
        @test read!(b) == 3.
    end
end

let b = RingBuffer{Float64}(5)
    @testset "Overwrite" begin
        for i=1.:5.
            write!(b, i)
        end
        @test isfull(b)
        @test b.wptr == 1
        @test b.rptr == 1

        write!(b, 6.)
        @test b.wptr == 2
        @test b.rptr == 2
        write!(b, 7.)
        @test b.wptr == 3
        @test b.rptr == 3
        write!(b, 8.)
        @test b.wptr == 4
        @test b.rptr == 4
        @test isfull(b)

        @test read!(b) == 4.
        @test read!(b) == 5.
        @test read!(b) == 6.
        @test read!(b) == 7.
        @test read!(b) == 8.
        @test isempty(b)
    end
end
