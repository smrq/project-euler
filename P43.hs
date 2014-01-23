module P43 where
import Data.List

isDivisibleBy n divisor = n `mod` divisor == 0

fromDigits = foldl appendDigit 0
	where appendDigit n digit = 10*n + digit

substringDivisibles = [[a,b,c,d,e,f,g,h,i,j] |
	d <- [0,2..8],
	f <- [0,5],
	f /= d,
	[a,b,c,e,g,h,i,j] <- permutations $ [0..9] \\ [d,f],
	fromDigits [c,d,e] `isDivisibleBy` 3,
	fromDigits [e,f,g] `isDivisibleBy` 7,
	fromDigits [f,g,h] `isDivisibleBy` 11,
	fromDigits [g,h,i] `isDivisibleBy` 13,
	fromDigits [h,i,j] `isDivisibleBy` 17]
	where cat x y z = 100*x + 10*y + z

result = sum $ map fromDigits substringDivisibles