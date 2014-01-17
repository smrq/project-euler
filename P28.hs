module P28 where
main = print result

spiralDiagonals n =
	1 + sd' 1 2 ((n - 1) `div` 2)
	where
		sd' _ _ 0 = 0
		sd' currentNumber currentGap remainingRings =
			currentNumber + currentGap +
			currentNumber + (2*currentGap) +
			currentNumber + (3*currentGap) +
			currentNumber + (4*currentGap) +
			sd' (currentNumber + (4*currentGap)) (currentGap + 2) (remainingRings - 1)

result = spiralDiagonals 1001