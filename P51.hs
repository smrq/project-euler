module P51 where
import P49 (primesInRange)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import qualified Data.Map as M

nDigitPrimes n = primesInRange (10^(n-1)) ((10^n)-1)
combinations :: [a] -> Int -> [[a]]
combinations ls 0 = [[]]
combinations ls n = concat $ map (\c -> map (:c) ls) $ combinations ls (n-1)

allEqual :: (Eq a) => [a] -> Bool
allEqual ls = all (== (head ls)) (tail ls)

masks :: Int -> [String]
masks n = filter (not . allEqual) $ combinations ['*', '.'] n

maskMatches :: String -> String -> Bool
maskMatches mask item =
	allEqual $ map snd $ filter (\(m,_) -> m == '*') $ zip mask item

primesMatching :: Int -> [(String, [Int])]
primesMatching n = map (\m -> (m, filter ((maskMatches m) . show) ps)) ms
	where
		ms = masks n
		ps = nDigitPrimes n

groupAllBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupAllBy f = M.toList . M.fromListWith (++) . map (f &&& return)

groupedPrimesByMask n =
	concat $ map groupPrimes $ primesMatching n
	where groupPrimes (mask,ps) = groupAllBy ((extractFromMask mask) . show) ps

extractFromMask mask item =
	map (\(m,c) -> if m == '*' then m else c) $ zip mask item

groupsWithLength l n = filter ((== l) . length . snd) $ groupedPrimesByMask n

result = minimum $ snd $ minimumBy (comparing (\(_,ps) -> minimum ps)) $ groupsWithLength 8 6