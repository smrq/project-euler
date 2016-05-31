module P12 where
main = print result

countDivisors :: Integer -> Integer
countDivisors 1 = 1
countDivisors x = 2 + countDivisors' x 2 -- 1 and X are always divisors
	where
		countDivisors' :: Integer -> Integer -> Integer
		countDivisors' x current
			| current*current == x	= 1
			| current*current >  x	= 0
			| x `rem` current == 0	= 2 + countDivisors' x (current+1)
			| otherwise           	= countDivisors' x (current+1)

triangles = scanl1 (+) [1..]

firstTriangleWithNDivisors n = head $ dropWhile (\t -> (countDivisors t) < n) triangles
result = firstTriangleWithNDivisors 500