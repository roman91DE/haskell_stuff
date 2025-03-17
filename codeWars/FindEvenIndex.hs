module FindEvenIndex (findEvenIndex) where

findEvenIndex :: [Int] -> Int
findEvenIndex arr = if null res then -1 else fst $ head res
  where
    res = [(i, x) | (i, x) <- zip [0 ..] arr, sumFirst i == sumTail i]
    sumFirst i = sum $ take i arr
    sumTail i = sum $ drop (i + 1) arr