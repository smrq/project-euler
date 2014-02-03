module P64 where
import P10 (diffSorted)
import Data.List ((\\))

squares :: (Integral int) => [int]
squares = map (^2) [1..]
nonSquares = diffSorted [1..] squares

-- http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
sqrtFractionExpansion :: Int -> [Int]
sqrtFractionExpansion n = loop 0 1 (floor $ sqrt $ fromIntegral n)
	where
		loop :: Int -> Int -> Int -> [Int]
		loop m d a = loop' m d a a
		loop' m d a a0 = a:(if a == 2*a0 then [] else loop' m' d' a' a0)
			where
				m' = d*a - m
				d' = (n - m'^2) `div` d
				a' = (a0 + m') `div` d'

hasOddPeriod :: [Int] -> Bool
hasOddPeriod expansion = even $ length expansion

result = length $ filter (hasOddPeriod . sqrtFractionExpansion) $ takeWhile (<= 10000) nonSquares