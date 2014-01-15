module P16 where
import Data.Char
main = print result

powerDigitSum n power = sum $ map digitToInt $ show (n ^ power)
result = powerDigitSum 2 1000