module TopWords (top3) where

import Data.Char
import Data.List (group, groupBy, sort, sortBy)
import Data.Ord (Down (Down), comparing)

isSplitSign :: Char -> Bool
isSplitSign c
  | isLetter c || c == '\'' = False
  | otherwise = True

toLowercase :: String -> String
toLowercase = map toLower

splitStr :: String -> [String]
splitStr = foldOp . toLowercase
  where
    foldOp = foldr (\x acc -> if isSplitSign x then [] : acc else (x : head acc) : tail acc) [[]]

filterOp :: [String] -> [String]
filterOp = filter (any isLetter) . filter (not . null)

topN :: Int -> String -> [String]
topN n s = map head topN
  where
    words = filterOp $ splitStr s
    grouped = group $ sort words
    sorted = sortBy (comparing (Data.Ord.Down . length)) grouped
    topN = take n sorted

top3 = topN 3
