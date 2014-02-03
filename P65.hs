module P65 where
import Data.Char (digitToInt)
import Data.Ratio (numerator, (%))

eSeq :: Int -> Int
eSeq 1 = 2
eSeq n
	| n `mod` 3 == 0 = 2 * (n `div` 3)
	| otherwise      = 1

eExpansion :: Int -> Rational
eExpansion n = eExpansion' n 0 where
	eExpansion' :: Int -> Rational -> Rational
	eExpansion' 1 acc = 2 + acc
	eExpansion' n acc = eExpansion' (n-1) (1 / ((fromIntegral (eSeq n)) + acc))

expansionSum :: Int -> Int
expansionSum = sum . (map digitToInt) . show . numerator . eExpansion

result = expansionSum 100