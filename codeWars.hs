{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}

import Data.Char (isAscii, isLetter, ord, toLower)
import Data.List (nub)
import Data.Set (fromList)

disemvowel :: String -> String
disemvowel str = [s | s <- str, s `notElem` vowels]
  where
    vowels = "aeiouAEIOU"

alphabetPosition :: String -> String
alphabetPosition str = unwords $ map (show . (\x -> ord x - magic)) sanitized
  where
    sanitized = [toLower s | s <- str, isAscii s, isLetter s]
    magic = 96

numOccurences :: (Eq a) => a -> [a] -> Int
numOccurences elem = foldr (\x acc -> if x == elem then acc + 1 else acc) 0

duplicateCount :: String -> Int
duplicateCount xs = length $ nub [c | c <- s, numOccurences c s > 1]
  where
    s = map toLower xs

popGrowth :: Int -> Double -> Int -> [Int]
popGrowth p0 percent aug = iterate (\p -> nextYear p percent aug) p0
  where
    nextYear :: Int -> Double -> Int -> Int
    nextYear p0 percent aug = p0 + floor (percent / 100 * fromIntegral p0) + aug

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = length $ takeWhile (< p) popSizes
  where
    popSizes = popGrowth p0 percent aug

findEvenIndex :: [Int] -> Int
findEvenIndex arr = if null res then -1 else fst $ head res
  where
    res = [(i, x) | (i, x) <- zip [0 ..] arr, sumFirst i == sumTail i]
    sumFirst i = sum $ take (i + 1) arr
    sumTail i = sum $ drop i arr
