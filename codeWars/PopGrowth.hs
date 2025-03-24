module PopGrowth (popGrowth, nbYear) where

popGrowth :: Int -> Double -> Int -> [Int]
popGrowth p0 percent aug = iterate (\p -> nextYear p percent aug) p0
  where
    nextYear :: Int -> Double -> Int -> Int
    nextYear p0 percent aug = p0 + floor (percent / 100 * fromIntegral p0) + aug

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = length $ takeWhile (< p) popSizes
  where
    popSizes = popGrowth p0 percent aug
    