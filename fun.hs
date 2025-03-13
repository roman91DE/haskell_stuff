groupConsecutive :: (Eq a) => [a] -> [[a]]
groupConsecutive = foldr (\x acc -> case acc of
                                      [] -> [[x]]
                                      (ys:yss) -> if x == head ys
                                                  then (x:ys) : yss
                                                  else [x] : acc) []


encode' :: (Eq a) => [a] -> [(a, Int)]
encode' [] = []
encode' xs = map (\ys -> (head ys, length ys)) grouped
    where 
        grouped = groupConsecutive xs