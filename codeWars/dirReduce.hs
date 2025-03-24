
data Direction = North | East | West | South deriving (Eq, Show)

isOpposed :: Direction -> Direction -> Bool
isOpposed d1 d2 = (d1, d2) `elem` [(North, South), (South, North), (East, West), (West, East)]

dirReduce :: [Direction] -> [Direction]
dirReduce = foldr reducer []
    where
        reducer :: Direction -> [Direction] -> [Direction]
        reducer d [] = [d]
        reducer d (x:xs) = if isOpposed d x then xs else d : (x:xs)


to1 = [North, South, West]
to3 = [North, East ,South]
