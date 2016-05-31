module P60 where
import Data.Array.Unboxed (UArray, accumArray, (!))
import Data.List (intersect, (\\))
import Data.Numbers.Primes (primes, isPrime)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

primes' :: [Int]
primes' = takeWhile (< 10000) (primes \\ [2,5])

isPrimePair :: Int -> Int -> Bool
isPrimePair a b =
	let a' = show a; b' = show b
	in (isPrime $ read $ a' ++ b') && (isPrime $ read $ b' ++ a')

primePairsMemo :: Map.Map Int [Int]
primePairsMemo = Map.fromList [(a, ps) |
	a <- primes',
	let ps = filter (isPrimePair a) $ filter (> a) primes']

sets :: Int -> [[Int]]
sets size = sets' [] primes' where
	sets' selected _ | length selected == size = [selected]
	sets' _ [] = []
	sets' selected ps =
		(sets' (p:selected) ps') ++ (sets' selected (tail ps))
		where
			p = head ps
			ps' = intersect ps $ fromJust $ Map.lookup p primePairsMemo

sums :: Int -> [Int]
sums = map sum . sets

result = minimum $ sums 5