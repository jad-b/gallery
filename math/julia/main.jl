function fib(n)
    a, b = 1, 0
    for i in n:-1:1
        a, b = a + b, a
    end
    b
end

euclids(a, b) = b == 0 ? a : euclids(b, a % b)
