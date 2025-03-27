module Codewars.Kata.Hashtag where

import Data.Char (isSpace, toUpper, isAlphaNum)

generateHashtag :: String -> Maybe String
generateHashtag s 
    | len > 140 = Nothing
    | len == 1 = Nothing
    | otherwise = Just hashtag
    where
        tokens = map capitalizeFirst $ words s
        nonEmptyTokens = filter (all isAlphaNum) tokens
        hashtag = '#' : concat nonEmptyTokens
        len = length hashtag

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs