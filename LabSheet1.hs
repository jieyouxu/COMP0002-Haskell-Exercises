{-|
    Haskell Exercises 1

    Note that functions are implemented with respect to the question 
    requirements, and are not necessarily the most efficient / straightforward
    implementations possible. 
-}

import Data.List
import Data.Char

square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth x y = (square x) + (square y)

isTriple :: Int -> Int -> Int -> Bool
isTriple adj opp hyp
    | adj + opp > hyp = (pyth adj opp) == (square hyp)
    | otherwise       = False

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny a b c
    =  (isTriple a b c) 
    || (isTriple a c b)
    || (isTriple b a c)
    || (isTriple b c a)
    || (isTriple c a b)
    || (isTriple c b a) 

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

halfIfEven :: Int -> Int
halfIfEven n
    | isEven n  = n `div` 2
    | otherwise = n

halfEvens :: [Int] -> [Int]
halfEvens xs =
    [ halfIfEven n | n <- xs ]

isInRange :: Int -> Int -> Int -> Bool
isInRange lowerBound upperBound n
    | (lowerBound > upperBound) = False
    | otherwise = (lowerBound <= n) && (n <= upperBound)

inRange :: Int -> Int -> [Int] -> [Int]
inRange lowerBound upperBound source
    = [ x | x <- source, withinBounds x ]
    where withinBounds n = isInRange lowerBound upperBound n

countPositives :: [Int] -> Int
countPositives source
    = length [ n | n <- source, isPositive n ]
    where isPositive n = n > 0

capitalised :: String -> String
capitalised str
    = (toUpper first) : [ toLower c | c <- rest ]
    where
        first = head str
        rest  = tail str

isLongWord :: String -> Bool
isLongWord word = (length word) >= 4

capitaliseLongWord :: String -> String
capitaliseLongWord word
    | isLongWord word = capitalised word
    | otherwise       = word

stringToLower :: String -> String
stringToLower str = [ toLower c | c <- str ]

title :: [String] -> [String]
title [] = []
title words
    = (capitalised firstWord) : [ capitaliseLongWord w | w <- lowerCaseWords ]
    where
        firstWord = head words 
        rest = tail words
        lowerCaseWords = [ stringToLower s | s <- rest ]
