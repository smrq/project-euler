module P62 where
import Data.List (nub, permutations, sort)
import qualified Data.Map as Map

incrementMap :: (Ord k) => Map.Map k Int -> k -> (Map.Map k Int, Int)
incrementMap map key =
	let value = 1 + (Map.findWithDefault 0 key map)
	in (Map.insert key value map, value)

cubes :: (Integral int) => [int]
cubes = map (^3) [1..]

ofLength :: (Integral int) => Int -> [int] -> [int]
ofLength len = takeWhile (< 10^len) . dropWhile (< 10^(len-1))

cubesWithDigits :: String -> [Integer]
cubesWithDigits str =
	filter (((sort str) ==) . sort . show) $ ofLength (length str) cubes

findPermutedCubes :: Int -> [Integer]
findPermutedCubes n = loop cubes (Map.empty) where
	loop ls map =
		let
			key = sort $ show $ head ls
			(map', count) = incrementMap map key
		in
			if n == count && n == (length $ cubesWithDigits key)
			then cubesWithDigits key
			else loop (tail ls) map'

result = minimum $ findPermutedCubes 5
