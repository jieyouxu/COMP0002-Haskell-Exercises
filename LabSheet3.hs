{-|
    Haskell Exercises 3

    Note that functions are implemented with respect to the question
    requirements, and are not necessarily the most efficient / straightforward
    implementations possible.
-}

mult :: [Int] -> Int
mult xs = foldr (*) 1 xs

posList :: [Int] -> [Int]
posList xs = filter (> 0) xs

trueList :: [Bool] -> Bool
trueList bs = foldr (&&) True bs
