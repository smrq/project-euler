module P33 where
import Data.List
import Data.Ratio
main = print result

cancelDigits :: (Int, Int) -> (Int, Int)
cancelDigits (a, b) =
	let
		a' = [a `div` 10, a `mod` 10]
		b' = [b `div` 10, b `mod` 10]
	in (head $ a' \\ nub b', head $ b' \\ nub a')

shareNonTrivialDigit :: (Int, Int) -> Bool
shareNonTrivialDigit (a, b) =
	let
		a' = [a `div` 10, a `mod` 10]
		b' = [b `div` 10, b `mod` 10]
		sharedDigits = intersect (nub a') (nub b')
	in length sharedDigits == 1 && head sharedDigits /= 0

isCurious fraction@(num, denom) =
	let (cNum, cDenom) = cancelDigits fraction
	in num * cDenom == cNum * denom

curiousFractions = [(a,b) | a <- [10..99], b <- [a+1..99], shareNonTrivialDigit (a, b), isCurious (a, b)]
curiousProduct = foldr1 (\(n1, d1) (n2, d2) -> (n1*n2, d1*d2)) curiousFractions
result = denominator (fst curiousProduct % snd curiousProduct)