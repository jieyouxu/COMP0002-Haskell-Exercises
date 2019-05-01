{-|
    Haskell Exercises 2

    Note that functions are implemented with respect to the question
    requirements, and are not necessarily the most efficient / straightforward
    implementations possible.
-}
import           Data.Char

isInRange :: Int -> Int -> Int -> Bool
isInRange lowerBound upperBound n = (lowerBound <= n) && (n <= upperBound)

inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange lowerBound upperBound (x:xs)
  | (lowerBound > upperBound) = []
  | withinBounds x = x : (inRange lowerBound upperBound xs)
  | otherwise = inRange lowerBound upperBound xs
  where
    withinBounds n = isInRange lowerBound upperBound n

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs)
  | isPositive x = 1 + (countPositives xs)
  | otherwise = countPositives xs
  where
    isPositive n = n > 0

stringToLower :: String -> String
stringToLower []     = []
stringToLower (c:cs) = (toLower c) : (stringToLower cs)

capitalised :: String -> String
capitalised []     = []
capitalised (c:cs) = (toUpper c) : (stringToLower cs)

titleHelper :: [String] -> [String]
titleHelper [] = []
titleHelper (w:ws)
  | isLongWord w = (capitalised w) : (titleHelper ws)
  | otherwise = w : (titleHelper ws)
  where
    isLongWord s = length s >= 4

title :: [String] -> [String]
title [] = []
title (w:ws) = (capitalised firstWord) : (titleHelper rest)
  where
    firstWord = stringToLower w
    rest = map stringToLower ws

isortHelper :: Ord a => [a] -> [a] -> [a]
isortHelper [] (u:unsorted) = isortHelper (u : []) unsorted
isortHelper sorted (u:unsorted) = isortHelper inserted unsorted
  where
    inserted = filter (< u) sorted ++ [u] ++ filter (>= u) sorted
isortHelper sorted [] = sorted

isort :: Ord a => [a] -> [a]
isort [] = []
isort xs = isortHelper [] xs

mergeHelper :: Ord a => [a] -> [a] -> [a] -> [a]
mergeHelper [] [] [] = []
mergeHelper accumulator [] [] = accumulator
mergeHelper accumulator xs [] = accumulator ++ xs
mergeHelper accumulator [] ys = accumulator ++ ys
mergeHelper accumulator (x:[]) (y:[])
  | x > y = mergeHelper (accumulator ++ [y]) (x : []) []
  | otherwise = mergeHelper (accumulator ++ [x]) [] (y : [])
mergeHelper accumulator (x:xs) (y:ys)
  | x > y = mergeHelper (accumulator ++ [y]) (x : xs) ys
  | otherwise = mergeHelper (accumulator ++ [x]) xs (y : ys)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs ys = mergeHelper [] xs ys

midIndexOf :: [a] -> Int
midIndexOf [] = 0
midIndexOf xs = (length xs) `div` 2

takeFirstHalf :: [a] -> [a]
takeFirstHalf [] = []
takeFirstHalf xs = take m xs
  where
    m = midIndexOf xs

takeSecondHalf :: [a] -> [a]
takeSecondHalf [] = []
takeSecondHalf xs = drop m xs
  where
    m = midIndexOf xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = (x : [])
msort xs = merge (msort firstHalf) (msort secondHalf)
  where
    firstHalf = takeFirstHalf xs
    secondHalf = takeSecondHalf xs

rotor :: Int -> [a] -> [a]
rotor offset []
  | offset < 0 = error "Offset cannot be smaller 0"
  | offset >= 0 =
    error "Offset cannot be larger than or equal to length of list"
  | otherwise = []
rotor 0 xs = xs
rotor offset (x:xs)
  | offset < 0 = error "Offset cannot be smaller 0"
  | offset >= (length (x : xs)) =
    error "Offset cannot be larger than or equal to length of list"
  | offset == 0 = xs
  | otherwise = rotor (offset - 1) (xs ++ [x])

alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

makeKey :: Int -> [(Char, Char)]
makeKey offset = [pair | pair <- zip alphabet offsetAlphabet]
  where
    offsetAlphabet = rotor offset alphabet
