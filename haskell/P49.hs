module P49 where
import Data.List
import Data.Array.Unboxed
import P27 (isPrime)

primesInRange :: Int -> Int -> [Int]
primesInRange a b =
	(if a<3 then [2] else []) ++ [n | n <- [a',a'+2..b], arr ! n]
	where
		a' = max (if even a then a+1 else a) 3
		pBound = floor $ sqrt $ fromIntegral b + 1
		arr :: UArray Int Bool
		arr = accumArray (\a b -> False) True (a', b)
			[(multiple,()) |
				p <- [3,5..pBound],
				let pSq = p*p,
				let s = 2*p,
				let (n,x) = quotRem (a' - pSq) s,
				let pSq' = if a' <= pSq then pSq else pSq + (n + signum x)*s,
				multiple <- [pSq',pSq'+s..b]]

arePermutations ls = and $ map (== (sort $ show $ head ls)) (map (sort . show) $ tail ls)
fourDigitPrimes = primesInRange 1000 9999
sequences = [[a,b,c] | a <- fourDigitPrimes, b <- fourDigitPrimes, b > a, let c = b+(b-a), c `elem` fourDigitPrimes, arePermutations [a,b,c]]
result = foldr1 (++) $ map show $ sequences !! 1