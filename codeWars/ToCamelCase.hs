module ToCamelCase (toCC, toCCFirst, splitOn, toCamelCase) where

import Data.Char (toUpper, toLower)

toCC :: String -> String
toCC [] = []
toCC (x : xs) = toUpper x : map toLower xs

toCCFirst :: String -> String
toCCFirst [] = []
toCCFirst (x : xs) = x : map toLower xs

splitOn :: String -> String -> [String]
splitOn cs = foldr (\x acc -> if x `elem` cs then "" : acc else (x : head acc) : tail acc) [""]

toCamelCase :: String -> String
toCamelCase str = first ++ rest
    where
        allWords = splitOn "_-" str
        (a, b) = (head allWords, tail allWords)
        first = toCCFirst a
        rest = concatMap toCC b