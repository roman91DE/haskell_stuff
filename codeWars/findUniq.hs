-- getUniqueSlow :: Eq a => [a] -> a
-- getUniqueSlow (x:xs) = if x `elem` xs then getUniqueSlow (xs ++ [x]) else x

-- import Data.List (group, sort)

-- getUniqueLessSlow :: Ord a => [a] -> a
-- getUniqueLessSlow  = head . head . filter (\xs -> length xs == 1) . group . sort

import qualified Data.Map.Strict as M

getUnique :: Ord a => [a] -> a
getUnique xs = fst. head . filter (\(_, c) -> c == 1) $ counts
    where
        counts = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])
