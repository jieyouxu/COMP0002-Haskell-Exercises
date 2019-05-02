{-
    Lab Sheet 4
-}

import           Data.Char

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
wordsHelper []        accumulator = accumulator
wordsHelper (c : [] ) [[]]        = [[c]]
wordsHelper (c : str) [[]]        = wordsHelper str [[c]]
wordsHelper (c : str) accumulator
    | isSpace c = wordsHelper str (accumulator ++ [[]])
    | otherwise = wordsHelper str (previousWords ++ [newWord])
    where
        previousWords = init accumulator
        newWord       = (last accumulator) ++ [c]

words' :: String -> [String]
words' []       = []
words' sentence = wordsHelper sentence [[]]

combineWords :: String -> String -> String
combineWords [] [] = []
combineWords l  [] = l
combineWords [] r  = r
combineWords l  r  = l ++ " " ++ r

unwords' :: [String] -> String
unwords' []    = []
unwords' [[]]  = []
unwords' words = foldr combineWords [] words

{-
    Reverse Polish Notation

    Only supports simple expressions of the form "int op int op int ...". No 
    support for operator precendence or parentheses. By default all four 
    operators are left-associative and are of equal precedence.

    1 + 2 * 3

    Input           Accumulator     Operator
    [1 + 2 * 3]     []              [ ]
    [  + 2 * 3]     [1]             [ ]
    [    2 * 3]     [1]             [+]
    [      * 3]     [1 2]           [+]
    [      * 3]     [1 2 +]         [ ]
    [        3]     [1 2 +]         [*]
    [         ]     [1 2 + 3]       [*]
    [         ]     [1 2 + 3 *]     [ ]
-}

isOperator :: String -> Bool
isOperator s = 
    c == '+' || c == '-' || c == '*' || c == '/'
    where
        c = head s

standard2RPNHelper :: [String] -> [String] -> [Char] -> [String]
standard2RPNHelper [] accumulator []        = accumulator
standard2RPNHelper [] accumulator (op : []) = accumulator ++ [[op]]
standard2RPNHelper (x : []) accumulator (op : []) =
    standard2RPNHelper [] (accumulator ++ [x]) [op]
standard2RPNHelper (x : xs) accumulator []
    | isOperator x = standard2RPNHelper xs accumulator x
    | otherwise    = standard2RPNHelper xs (accumulator ++ [x]) []
standard2RPNHelper (x : xs) accumulator (op : [])
    | isOperator x = standard2RPNHelper xs (accumulator ++ [[op]]) x
    | otherwise    = standard2RPNHelper xs (accumulator ++ [x]) [op]

standard2RPN :: String -> String
standard2RPN []       = []
standard2RPN sentence = 
    unwords' $ standard2RPNHelper (words' sentence) [] []
