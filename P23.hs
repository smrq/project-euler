module P23 where
import P21 (properDivisors)
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
main = print result

abundantLimit = 28123

isAbundant :: Int -> Bool
isAbundant x = (sum $ properDivisors x) > x

abundants :: [Int]
abundants = [n | n <- [1..abundantLimit], isAbundant n]

isAbundantSumPairs :: [(Int, Bool)]
isAbundantSumPairs = runST $ do
	arr <- newArray (1, abundantLimit) False :: ST s (STUArray s Int Bool)
	forM_ abundants $ \ab1 ->
		forM_ abundants $ \ab2 ->
			let abSum = ab1 + ab2
			in when (abSum <= abundantLimit) $ writeArray arr abSum True
	getAssocs arr

notAbundantSums :: [Int]
notAbundantSums = map fst $ filter (not . snd) isAbundantSumPairs

result = sum notAbundantSums