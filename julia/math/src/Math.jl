function simpsonsrule(f, a, b, n)
    h = (b-a)/n
    # The term coefficient
    function coef(k)
        if k == 0 || k == n
            1
        elseif k % 2 == 0
            2
        else
            4
        end
    end
    # The term itself
    term(k) = coef(k) * f(a + k * h)
    (h/3) * Σ(term, 1:n)
end


function Σ(fn, iter)
    mapreduce(fn, +, iter)
end

function fib(n)
    a, b = 1, 0
    for i in n:-1:1
        a, b = a + b, a
    end
    b
end

euclids(a, b) = b == 0 ? a : euclids(b, a % b)
