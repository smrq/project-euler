module P39 where
import Data.List (maximumBy)
import Data.Ord (comparing)

triangles p = [(a,b,c) |
	let bound = p `div` 2,
	a <- [1..bound],
	b <- [a..bound],
	let c = p-a-b,
	c > b,
	a^2 + b^2 == c^2]
result = maximumBy (comparing $ length . triangles) [1..1000]