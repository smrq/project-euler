module P61 where

nGonal :: Int -> Int -> Int
nGonal n i = i*((n-2)*i - (n-4)) `div` 2

fourDigit :: [Int] -> [Int]
fourDigit ordered = takeWhile (< 10^4) $ dropWhile (< 10^3) ordered

isCyclicSuccessor :: Int -> Int -> Bool
isCyclicSuccessor n q =	n `mod` 100 == q `div` 100

deleteAt :: Int -> [a] -> [a]
deleteAt n ls = (take n ls) ++ (drop (n+1) ls)

mapWithRest :: (a -> [a] -> b) -> [a] -> [b]
mapWithRest f ls = mapWithRest' 0 ls where
	mapWithRest' n ls
		| n == length ls = []
		| otherwise = (f (ls !! n) (deleteAt n ls)) : mapWithRest' (n+1) ls

cycles :: [[Int]] -> [[Int]]
cycles sets = concat $ map (\n -> f [n] (tail sets)) (head sets) where
	f selected [] =
		if (isCyclicSuccessor (head selected) (last selected))
		then [selected] else []
	f selected sets = concat $ mapWithRest (f' selected) sets where
		f' selected set rest =
			concat $ map (\s -> f (s:selected) rest) $ filter (isCyclicSuccessor (head selected)) set

result = sum $ head $ cycles $ map (\ng -> fourDigit $ map ng [1..]) $ map nGonal [3..8]