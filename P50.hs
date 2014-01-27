module P50 where
import P10 (primes)
import P27 (isPrime)

bestPrimeBelow bound skipN =
	last $ takeWhile (< bound) $ filter isPrime sums
	where sums = map (\n -> sum $ take n $ drop skipN primes) [21..]

-- this gets the right result, but not provably so
result = maximum $ map (bestPrimeBelow 1000000) [0..10]