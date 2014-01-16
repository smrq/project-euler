module P24 where
main = print result

fac :: Int -> Int
fac 1 = 1
fac n = n * (fac $ n - 1)

deleteAt :: Int -> [a] -> [a]
deleteAt n ls = let (front, back) = splitAt n ls in front ++ (tail back)

remainingDigit :: Int -> Int -> Int
remainingDigit n 0 = 0
remainingDigit n digit = n `mod` (fac $ digit + 1) `div` (fac digit)

lexPermIndices :: Int -> [Int]
lexPermIndices n = [remainingDigit n d | d <- [9,8..0]]

lexPerm :: Int -> [Int]
lexPerm n =
	lexPerm' (lexPermIndices (n-1)) [0..9]
	where
		lexPerm' :: [Int] -> [Int] -> [Int]
		lexPerm' [] _ = []
		lexPerm' (index:rest) remainingDigits =
			(remainingDigits !! index):(lexPerm' rest (deleteAt index remainingDigits))

result = lexPerm 1000000