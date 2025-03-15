quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort greaterOrEqual
  where
    smaller = filter (<x) xs
    greaterOrEqual = filter (>=x) xs


unord :: [Int]
unord = [9,1,5,3,6]

sorted :: [Int]
sorted = quicksort unord

main :: IO ()
main = print sorted
