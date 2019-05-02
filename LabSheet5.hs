-- pre:     n >= 0
-- post:    fac n = n!
fac :: Int -> Int
fac 0 = 1
fac n 
    | n < 0     = error "n must be non-negative"     
    | otherwise = n * (fac (n - 1))

-- pre:     n >= 0
-- post:    fib n = fibonacci(n)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n
    | n < 0     = error "n must be non-negative"
    | otherwise = fib (n - 1) + fib (n - 2)
