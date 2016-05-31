module P14 where
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)
main = print result

-- Naive
--collatz 1 = 1
--collatz n
--	| even n = 1 + (collatz (n `div` 2))
--	| otherwise = 1 + (collatz (n*3 + 1))

-- Memoized
memoLimit = 1000000
collatzMemo = array (1, memoLimit) [(n, collatz n) | n <- [1..memoLimit]]

collatz :: Integer -> Int
collatz 1 = 1
collatz n = 1 + recur nextN
	where
		nextN = if even n then (n `div` 2) else (3 * n + 1)
		recur = if nextN <= memoLimit then (collatzMemo !) else collatz

result = maximumBy (comparing collatz) [1..1000000]