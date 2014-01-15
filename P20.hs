module P20 where
import Data.Char
main = print result
result = sum $ map digitToInt $ show $ foldr (*) 1 [1..100]