module P65 where
import Data.Char (digitToInt)
import Data.Ratio (numerator, denominator, (%))

convergent :: [Integer] -> Int -> Rational
convergent seq = f
	where
		f = (map convergent' [0..] !!)
		a0 = seq !! 0
		a1 = seq !! 1
		convergent' 0 = a0 % 1
		convergent' 1 = (a1*a0 + 1) % a1
		convergent' n =
			let
				an = seq !! n
				cn_1 = f $ n-1
				cn_2 = f $ n-2
				nn_1 = numerator cn_1
				nn_2 = numerator cn_2
				dn_1 = denominator cn_1
				dn_2 = denominator cn_2
			in (an * nn_1 + nn_2) % (an * dn_1 + dn_2)

eTerms :: [Integer]
eTerms = 2 : (foldr1 (++) $ map (\k -> [1,2*k,1]) [1..])

eConvergent :: Int -> Rational
eConvergent = convergent eTerms

result = sum $ map digitToInt $ show $ numerator $ eConvergent 99