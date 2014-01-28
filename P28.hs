module P28 where
main = print result

spiralDiagonalRing 1 = [1]
spiralDiagonalRing ring = [x-3*d, x-2*d, x-d, x] where x = (2*ring-1)^2; d = 2*ring-2
spiralDiagonals n = concat $ map spiralDiagonalRing [1..1+((n-1) `div` 2)]

result = sum $ spiralDiagonals 1001
