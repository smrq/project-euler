module P38 where
import P32 (areDigitsUniqueIn, areDigitsCompleteIn)
import Data.List

maxPandigital = 987654321
maxMultiplicand = 10^((length $ show maxPandigital) `div` 2) - 1

isPandigital :: [Int] -> String -> Bool
isPandigital digits n = (areDigitsUniqueIn digits n) && (areDigitsCompleteIn digits n)

concatWithMultiples :: Int -> String
concatWithMultiples x =
	last $ takeWhile ((<= 9) . length) $ scanl1 (++) $ map (show . (x *)) [1..]

pandigitalMultiples :: [Int]
pandigitalMultiples = map read $
	filter (isPandigital [1..9]) $
		map concatWithMultiples [1..maxMultiplicand]

result = maximum pandigitalMultiples