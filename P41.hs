module P41 where
import P10 (primes)
import P38 (isPandigital)
import Data.Char
import Data.Function

-- Any pandigital number with N digits is divisible by 3, with N <- [2,3,5,6,8,9]
isDivisibleBy3 :: Int -> Bool
isDivisibleBy3 number = repeatedDigitSum number `mod` 3 == 0
	where repeatedDigitSum x = (fix $ \f x ->
		if x <= 9 then x else f $ sum $ map digitToInt $ show x) x

candidates = takeWhile (< 7654321) primes
result = maximum $ filter (\n -> isPandigital [1..length $ show n] $ show n) candidates