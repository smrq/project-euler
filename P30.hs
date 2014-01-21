module P30 where
import Data.Char
main = print result

sumPower n power = sum (map ((^ power) . digitToInt) (show n))
maxDigits power = head $ dropWhile (\n -> let nines = 10^n-1 in nines < sumPower nines power) [1..]

equalToSumOfNthPowers power = filter (\n -> n == sumPower n power) [2..10^(maxDigits power)-1]
result = sum $ equalToSumOfNthPowers 5