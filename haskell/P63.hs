module P63 where
import P62 (ofLength)

nthPowers :: Int -> [Integer]
nthPowers n = map (^n) [1..]

powerful :: Int -> Int
powerful n = length $ ofLength n $ nthPowers n

result = sum $ takeWhile (> 0) $ map powerful [1..]
