{-|
    Haskell Exercises 3

    Note that functions are implemented with respect to the question
    requirements, and are not necessarily the most efficient / straightforward
    implementations possible.
-}

mult :: [Int] -> Int
mult xs = foldr (*) 1 xs
