module P53 where
import P34 (fac)
ncr n r = (fac n) `div` ((fac r) * (fac $ n - r))
result = length $ [() | n <- [23..100], r <- [1..n], ncr n r > 1000000]