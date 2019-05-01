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

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs)
    | isPositive x = 1 + (countPositives xs)
    | otherwise    = countPositives xs
    where
        isPositive n = n > 0

stringToLower :: String -> String
stringToLower [] = []
stringToLower (c:cs) = (toLower c) : (stringToLower cs)

capitalised :: String -> String
capitalised [] = []
capitalised (c:cs) = (toUpper c) : (stringToLower cs)

titleHelper :: [String] -> [String]
titleHelper [] = []
titleHelper (w:ws)
    | isLongWord w = (capitalised w) : (titleHelper ws)
    | otherwise    = w : (titleHelper ws)
    where 
        isLongWord s = length s >= 4  

title :: [String] -> [String]
title [] = []
title (w:ws)
    = (capitalised firstWord) : (titleHelper rest)
    where
        firstWord = stringToLower w
        rest = map stringToLower ws
