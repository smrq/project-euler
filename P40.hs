module P40 where
import Data.Char

digits = foldr1 (++) $ map show [1..]
result = product $ map (digitToInt . (digits !!)) [0, 9, 99, 999, 9999, 99999, 999999]