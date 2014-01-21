module P10 where
main = print result

primes :: [Int]
primes = [2,3,5] ++ (diffSorted [7,9..] oddComposites)

oddComposites :: [Int]
oddComposites = mergeAllSorted $ map multiples $ tail primes
	where multiples p = [n*p | n <- [p,p+2..]]

-- Removes all elements in the second (potentially infinite) sorted list from the first
diffSorted :: (Ord a) => [a] -> [a] -> [a]
diffSorted [] ys = []
diffSorted xs [] = xs
diffSorted xs@(x:xt) ys@(y:yt) =
	case compare x y of
		LT -> x:(diffSorted xt ys)
		EQ -> diffSorted xt yt
		GT -> diffSorted xs yt

-- Merges two (potentially infinite) sorted lists
mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted xs@(x:xt) ys@(y:yt) =
	case compare x y of
		LT -> x:(mergeSorted xt ys)
		EQ -> x:(mergeSorted xt yt)
		GT -> y:(mergeSorted xs yt)

mergeAllSorted :: (Ord a) => [[a]] -> [a]
mergeAllSorted lists = foldr1 f lists
	where f (x:xt) ys = x:(mergeSorted xt ys)

primesBelowTwoMillion = takeWhile (< 2000000) primes
result = sum primesBelowTwoMillion