module P26 where
import Data.List (maximumBy)
import Data.Ord (comparing)
main = print result

lengthen remainder divisor
	| remainder >= divisor = (remainder, 0)
	| otherwise = let p = (lengthen (10 * remainder) divisor) in (fst p, (snd p) + 1)

cycles :: Int -> Int
cycles n =
	longDivide 1 n []
	where
		longDivide :: Int -> Int -> [(Int, Int)] -> Int
		longDivide remainder divisor previousRemainders =
			let
				(dividend, count) = lengthen remainder divisor
				(nonMatching, rest) = span (\a -> dividend /= (fst a)) previousRemainders
			in
				if not $ null rest
				then count + (sum $ map snd nonMatching)
				else
					let remainder = dividend `mod` divisor
					in
						if remainder == 0 then 0
						else longDivide remainder divisor ((dividend, count) : previousRemainders)

cycleResults = [(n, cycles n) | n <- [2..999]]
result = fst $ maximumBy (comparing snd) cycleResults