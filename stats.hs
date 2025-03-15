{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.List (sort)
import Data.Maybe (isJust, fromMaybe)

arithMean :: (Real a, Fractional b) => [a] -> Maybe b
arithMean [] = Nothing
arithMean xs = Just $ realToFrac sumL / fromIntegral lenL
  where
    sumL = sum xs
    lenL = length xs

median :: (Ord a, Real a, Fractional b) => [a] -> Maybe b
median [] = Nothing
median xs
  | odd len = Just $ realToFrac (ys !! mid)
  | otherwise = Just $ (realToFrac (ys !! (mid - 1)) + realToFrac (ys !! mid)) / 2
  where
    ys = sort xs
    len = length xs
    mid = len `div` 2


deviations :: (Real a, Fractional b) => [a] -> [b]
deviations [] = []
deviations xs = case arithMean xs of
    Just mean -> map (\x -> realToFrac x - mean) xs
    Nothing   -> []  -- Handle the empty case


meanAbsDev :: (Real a, Fractional b) => [a] -> Maybe b
meanAbsDev [] = Nothing
meanAbsDev xs = Just (sum (map abs devs) * (1 / len))
  where
    devs = deviations xs
    len = realToFrac $ length xs

variance :: (Real a, Fractional b) => [a] -> Maybe b
variance [] = Nothing
variance xs = Just (sum sqDevs * (1 / len))
  where
    sqDevs = map (^ 2) $ deviations xs
    len = realToFrac $ length xs

stdDev :: (Real a, Floating b) => [a] -> Maybe b
stdDev xs = fmap sqrt var
  where
    var = variance xs
