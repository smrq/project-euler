module P37 where
import P10 (primes)
import P27 (isPrime)
import Data.List

truncations :: Int -> [Int]
truncations n =
	sort $ map read $ delete "" $ union (truncationsL' (show n)) (truncationsR' (show n))
	where
		truncationsL' str = scanr (:) [] str
		truncationsR' str = map reverse $ scanl (flip (:)) [] str

truncatablePrimes = take 11 [p | p <- primes, p > 10, all isPrime $ truncations p]
result = sum truncatablePrimes