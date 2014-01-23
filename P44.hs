module P44 where
import P42 (isPerfectSquare)
import Debug.Trace

pentagonal n = n*(3*n-1) `div` 2

isPentagonalNumber n = (isPerfectSquare $ 24*n + 1) &&
	(floor $ sqrt $ fromIntegral $ 24*n + 1) `mod` 6 == 5

firstPentagonalPair :: (Integer, Integer, Integer)
firstPentagonalPair = loop 1 where
	loop j = case bestPentagonalSummand j 10000000 of
		Just pair -> pair
		Nothing -> loop (j+1)

-- This takes a long time? It should return firstPentagonalPair in the end, which is lame.
bestPentagonalPair :: (Integer, Integer, Integer)
bestPentagonalPair = loop (firstJ+1) firstPentagonalPair
	where
		(firstJ,_,_) = firstPentagonalPair
		loop :: Integer -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
		loop j bestPair@(bestJ, bestK, bestDiff)
			| pentDiff j (j+1) > bestDiff = bestPair
			| otherwise = case bestPentagonalSummand j bestDiff of
					Just pair -> loop (j+1) pair
					Nothing -> loop (j+1) bestPair

bestPentagonalSummand :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
bestPentagonalSummand j diffBound = loop (j+1) where
	loop k =
		let
			diff = pentDiff j k
			sum = pentSum j k
		in
			if diff >= diffBound then Nothing
			else if (isPentagonalNumber diff) && (isPentagonalNumber sum) then Just (j, k, diff)
			else loop (k+1)

pentSum j k = (pentagonal k) + (pentagonal j)
pentDiff j k = (pentagonal k) - (pentagonal j)