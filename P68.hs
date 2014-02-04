module P68 where
import Control.Applicative ((<*>))
import Data.List (nub, permutations, sort, zipWith)

zipAdjWith :: (a -> a -> b) -> [a] -> [b]
zipAdjWith f = (zipWith f) <*> tail

sumCyclePairs :: (Num a) => [a] -> [a]
sumCyclePairs = (zipWith (+)) <*> shift1

gaps :: (Num a, Ord a) => [a] -> [a]
gaps = (zipAdjWith (flip (-))) . sort

shift1 :: [a] -> [a]
shift1 ls = (tail ls) ++ [head ls]

areDistinct :: (Eq a) => [a] -> Bool
areDistinct ls = ls == (nub ls)

cycleFromLowest :: (Ord a) => [a] -> [a]
cycleFromLowest ls
	| (head ls) == (minimum ls) = ls
	| otherwise = cycleFromLowest $ shift1 ls

cyclePairsFromLowest :: (Ord a) => [(a,b)] -> [(a,b)]
cyclePairsFromLowest ls
	| (fst $ head ls) == (minimum $ map fst ls) = ls
	| otherwise = cyclePairsFromLowest $ shift1 ls

validCycles :: [Int] -> [Int] -> [[Int]]
validCycles inner outer =
	nub $ map cycleFromLowest $ filter (\c -> gapsComplement outer $ sumCyclePairs c) $ permutations inner
	where
		gapsComplement a b = (gaps a) == (reverse $ gaps b)

innerCycles = validCycles [1,2,3,4,5] [6,7,8,9,10]

-- The rest is trivial to figure out on paper.
-- The two cycles resulting in innerCycles yield the strings:
-- 6 3 5  7 5 2  8 2 4  9 4 1  10 1 3
-- 6 5 3  10 3 1  9 1 4  8 4 2  7 2 5