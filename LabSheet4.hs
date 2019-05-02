{-
    Lab Sheet 4
-}

import Data.Char

{-
    Target String: "this is a word"
    Output                          Original
    [[]]                            [this is a word]
    [[t]]                           [his is a word]
    [[th]]                          [is is a word]
    [[thi]]                         [s is a word]
    [[this]]                        [ is a word]
    [[this], []]                    [is a word]
    [[this], [i]]                   [s a word]
    [[this], [is]]                  [ a word]
    [[this], [is], []]              [a word]
    [[this], [is], [a]]             [ word]
    [[this], [is], [a], []]         [word]
    [[this], [is], [a], [w]]        [ord]
    [[this], [is], [a], [wo]]       [rd]
    [[this], [is], [a], [wor]]      [d]
    [[this], [is], [a], [word]]     []
-}
wordsHelper :: String -> [String] -> [String]
wordsHelper [] accumulator = accumulator
wordsHelper (c:[]) [[]] = [[c]]
wordsHelper (c:str) [[]] = wordsHelper str [[c]]
wordsHelper (c:str) accumulator
    | isSpace c = wordsHelper str (accumulator ++ [[]])
    | otherwise = wordsHelper str (previousWords ++ [newWord])
    where
        previousWords = init accumulator
        newWord = (last accumulator) ++ [c]

words' :: String -> [String]
words' [] = []
words' sentence = wordsHelper sentence [[]]

combineWords :: String -> String -> String
combineWords [] [] = []
combineWords l [] = l
combineWords [] r = r
combineWords l r  = l ++ " " ++ r

unwords' :: [String] -> String
unwords' [] = []
unwords' [[]] = []
unwords' words = foldr combineWords [] words
