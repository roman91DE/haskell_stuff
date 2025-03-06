abs' :: (Num a, Ord a) => a -> a
abs' n
    | n < 0 =  -n
    | otherwise = n
