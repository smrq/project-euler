module P32 where
import P21 (divisors)
import Data.Char
import Data.List
main = print result

isPandigitalProduct range n
	| not $ areDigitsUniqueIn range $ show n = False
	| otherwise = not $ null d'''
		where
			d = divisors n
			d' = filter (areDigitsUniqueIn range . nWithDivisor) d
			d'' = filter (areDigitsUniqueIn range . nWithBothDivisors) d'
			d''' = filter (areDigitsCompleteIn range . nWithBothDivisors) d'

			nWithDivisor :: Int -> String
			nWithDivisor divisor = (show n) ++ (show divisor)

			nWithBothDivisors :: Int -> String
			nWithBothDivisors divisor = (show n) ++ (show divisor) ++ (show (n `div` divisor))

			areDigitsUniqueIn :: [Int] -> [Char] -> Bool
			areDigitsUniqueIn digits n = (n == nub n) && (null $ n \\ (map intToDigit digits))

			areDigitsCompleteIn :: [Int] -> [Char] -> Bool
			areDigitsCompleteIn digits n = sort n == sort (map intToDigit digits)

result = sum $ filter (isPandigitalProduct [1..9]) [1..10000]