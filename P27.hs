module P27 where
import P10 (primes)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST
main = print result

bound = 100000
isPrimeArray :: UArray Int Bool
isPrimeArray = runSTUArray $ do
	arr <- newArray (1,bound) False :: ST s (STUArray s Int Bool)
	forM_ (takeWhile (< bound) primes) $ \p ->
		writeArray arr p True
	return arr

isPrime :: Int -> Bool
isPrime n
	| n <= 0 = False
	| n <= bound = isPrimeArray ! n
	| otherwise = n == head (dropWhile (< n) primes)

countPrimes :: (Int -> Int) -> Int
countPrimes fn = length $ takeWhile (isPrime . fn) [0..]

quad :: Int -> Int -> Int -> Int
quad a b n = n*n + a*n + b

params :: [(Int, Int)]
params = [(a, b) |
	b <- (takeWhile (< 1000) primes),
	a <- [-999..999],
	all (isPrime . quad a b) [1..40]]

bestParams = maximumBy (comparing (\(a, b) -> countPrimes $ quad a b)) params
result = (fst bestParams) * (snd bestParams)