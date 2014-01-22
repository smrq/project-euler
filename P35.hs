module P35 where
import P10 (primes)
import Data.Char
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST
--main = print result

rotate :: String -> String
rotate s = tail s ++ [head s]

allRotations :: Int -> [Int]
allRotations n = allRotations' n $ rotate $ show n
	where
		allRotations' n r
			| show n == r = [n]
			| otherwise = (read r) : allRotations' n (rotate r)

hasNoZeros n = all (/= '0') (show n)

bound = 999999
circularPrimeArray :: UArray Int Bool
circularPrimeArray = runSTUArray $ do
	arr <- newArray (1,bound) False :: ST s (STUArray s Int Bool)
	let boundedPrimes = filter hasNoZeros $ takeWhile (< bound) primes
	forM_ boundedPrimes $ \p -> writeArray arr p True
	forM_ boundedPrimes $ \p -> do
		let cycles = allRotations p
		areCyclesPrime <- mapM (readArray arr) cycles
		when (not $ all id areCyclesPrime) $ forM_ cycles $ \c -> writeArray arr c False
	return arr

circularPrimes = [i | i <- [1..bound], circularPrimeArray ! i]