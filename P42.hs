module P42 where
import P22 (parse, scoreLetters)
main = do
	fileContents <- readFile "P42_words.txt"
	let words = parse fileContents
	let scores = map scoreLetters words
	let triangleScores = filter isTriangleNumber scores
	print $ length triangleScores

isPerfectSquare n = sq * sq == n
	where sq = floor $ sqrt $ (fromIntegral n::Double)

--  t   = n(n+1)/2       ==>
-- 8t   = 4n^2 + 4n      ==>
-- 8t+1 = 4n^2 + 4n + 1  ==>
-- 8t+1 = (2n+1)^2
isTriangleNumber n = (isPerfectSquare $ 8*n + 1) &&
	(odd $ floor $ sqrt $ fromIntegral $ 8*n + 1)