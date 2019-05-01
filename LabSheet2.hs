{-|
    Haskell Exercises 2

    Note that functions are implemented with respect to the question 
    requirements, and are not necessarily the most efficient / straightforward
    implementations possible. 
-}

import Data.Char

isInRange :: Int -> Int -> Int -> Bool
isInRange lowerBound upperBound n
    = (lowerBound <= n) && (n <= upperBound)

inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange lowerBound upperBound (x:xs)
    | (lowerBound > upperBound) = []
    | withinBounds x            = x : (inRange lowerBound upperBound xs)
    | otherwise                 = inRange lowerBound upperBound xs
    where 
        withinBounds n = isInRange lowerBound upperBound n
