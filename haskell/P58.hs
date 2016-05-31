module P58 where
import P49 (primesInRange)
import P28 (spiralDiagonalRing)
import Data.List (intersect)

countPrimes ls = length $ intersect ls (primesInRange (head ls) (last ls))

ratios = scanl accum (3,5,3) [3..]
	where accum (ct, d, sideLength) n = (ct + (countPrimes ring), d + 4, sideLength + 2)
		where ring = spiralDiagonalRing n

-- takes basically forever
result = sideLength where (_,_,sideLength) = head $ dropWhile (\(n,d,_) -> n*10 >= d) ratios