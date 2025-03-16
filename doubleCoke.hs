queue = ["Penny", "Lennard", "Sheldon", "Rajesh"]

drink :: [String] -> [String]
drink [] = []
drink (x:xs) = xs ++ doubled
    where
        doubled = replicate 2 x


nthPerson :: Int -> [String] -> Maybe String
nthPerson _ [] = Nothing
nthPerson n xs = Just $ head $ last lastList
    where
        lastList = take n $ iterate drink xs