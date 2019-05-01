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

evenList :: [Int] -> Bool
evenList xs = trueList (map isEven xs)
  where
    isEven n = n `mod` 2 == 0

getLarger :: Ord a => a -> a -> a
getLarger x y
  | x > y = x
  | otherwise = y

maxList :: Ord a => [a] -> a
maxList []     = error "List cannot be empty"
maxList (x:xs) = foldr getLarger x (x : xs)

inRange :: Int -> Int -> [Int] -> [Int]
inRange lowerBound upperBound xs = filter withinBounds xs
  where
    withinBounds n = (lowerBound <= n) && (n <= upperBound)
