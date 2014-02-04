module P66 where
import P64 (nonSquares, sqrtFractionExpansion)
import P65 (convergent)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Ratio (numerator, denominator)

infiniteSqrtFractionExpansion n =
	let finiteExpansion = sqrtFractionExpansion n
	in (head finiteExpansion) : (cycle $ tail finiteExpansion)

quadDiophantine d = (numerator ratio, denominator ratio)
	where
		ratio = head $ filter valid convergents
		convergents = map (convergent (infiniteSqrtFractionExpansion d)) [1..]
		valid c = (numerator c)^2 - d*(denominator c)^2 == 1

result = maximumBy (comparing (fst . quadDiophantine)) $ takeWhile (<= 1000) nonSquares