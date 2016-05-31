module P45 where
import P42 (isPerfectSquare, isTriangleNumber)
import P44 (isPentagonalNumber)

isHexagonalNumber n = (isPerfectSquare $ 8*n + 1) &&
	(floor $ sqrt $ fromIntegral $ 8*n + 1) `mod` 4 == 3

hexagonals = map (\n -> n*(2*n - 1)) [1..]
result = [n | n <- hexagonals, isTriangleNumber n, isPentagonalNumber n] !! 2