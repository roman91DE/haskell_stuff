module DuplicateCount (duplicateCount) where

import Data.Char (toLower)
import Data.List (nub)

numOccurences :: (Eq a) => a -> [a] -> Int
numOccurences elem = foldr (\x acc -> if x == elem then acc + 1 else acc) 0

duplicateCount :: String -> Int
duplicateCount xs = length $ nub [c | c <- s, numOccurences c s > 1]
  where
    s = map toLower xs