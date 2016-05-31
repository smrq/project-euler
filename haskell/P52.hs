module P52 where
import P49 (arePermutations)
areMultiplesPermutations limit x = arePermutations [n*x | n <- [1..limit]]
result = head $ filter (areMultiplesPermutations 6) [1..]