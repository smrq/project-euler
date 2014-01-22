module P34 where
import Data.Char
main = print result

fac :: Int -> Integer
fac n = facs !! n
	where facs = scanl (*) 1 [1..]

sumFact n = sum $ map (fac . digitToInt) (show n)
maxDigits = head $ dropWhile (\n -> let nines = 10^n-1 in nines < sumFact nines) [1..]

digitFactorials = filter (\n -> n == sumFact n) [10..(sumFact $ 10^maxDigits-1)]
result = sum $ digitFactorials