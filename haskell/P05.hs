module P05 where
import P03 (primeFactors)
main = print result

factors = map primeFactors [1..20]

spliceFactors ls1 ls2
    | null ls1            = ls2
    | null ls2            = ls1
    | head ls1 < head ls2 = head ls1 : spliceFactors (tail ls1) ls2
    | head ls1 > head ls2 = head ls2 : spliceFactors ls1 (tail ls2)
    | otherwise           = head ls1 : spliceFactors (tail ls1) (tail ls2)

result = product $ foldr spliceFactors [] factors