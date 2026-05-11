x = 3

y = 2x

typeof(y)

f(x) = 2 + x


f

f(10)


function g(x, y)
    z = x + y
    return z^2
end

g(1, 2)

let s = 0
    for i in 1:10
        s += i    # Equivalent to s = s + i
    end
    s
end


typeof(1:10)

function mysum(n)
    s = 0
    for i in 1:n
        s += i
    end
    return s
end

mysum(100)

a = 3

a < 5

if a < 5
    "small"
else
    "big"
end

v = [1, 2, 3]

typeof(v)

v[2]

v[2] = 10

v2 = [i^2 for i in 1:10]

M = [1 2
     3 4]

typeof(M)

zeros(5, 5)

zeros(Int, 4, 5)

[i + j for i in 1:5, j in 1:6]
