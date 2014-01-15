module P17 where
import Data.Char
main = print result

place p x = x `div` (10^p) `mod` 10 * (10^p)
ones = place 0
tens = place 1
hundreds = place 2
thousands x = x `div` 1000 * 1000

readableNumber 0 = "zero"
readableNumber 1 = "one"
readableNumber 2 = "two"
readableNumber 3 = "three"
readableNumber 4 = "four"
readableNumber 5 = "five"
readableNumber 6 = "six"
readableNumber 7 = "seven"
readableNumber 8 = "eight"
readableNumber 9 = "nine"
readableNumber 10 = "ten"
readableNumber 11 = "eleven"
readableNumber 12 = "twelve"
readableNumber 13 = "thirteen"
readableNumber 14 = "fourteen"
readableNumber 15 = "fifteen"
readableNumber 16 = "sixteen"
readableNumber 17 = "seventeen"
readableNumber 18 = "eighteen"
readableNumber 19 = "nineteen"
readableNumber 20 = "twenty"
readableNumber 30 = "thirty"
readableNumber 40 = "forty"
readableNumber 50 = "fifty"
readableNumber 60 = "sixty"
readableNumber 70 = "seventy"
readableNumber 80 = "eighty"
readableNumber 90 = "ninety"
readableNumber x
	| hundreds x == x = (readableNumber (x `div` 100)) ++ " hundred"
	| thousands x == x = (readableNumber (x `div` 1000)) ++ " thousand"
	| x > 20 && x < 100 = (readableNumber $ tens x) ++ "-" ++ (readableNumber $ ones x)
	| x > 100 && x < 1000 = (readableNumber $ hundreds x) ++ " and " ++ (readableNumber $ x - (hundreds x))
	| x > 1000 = (readableNumber $ thousands x) ++ " " ++ (readableNumber $ x - (thousands x))

countLetters str = length $ filter isAlpha str

result = countLetters $ foldr (++) "" $ map readableNumber [1..1000]