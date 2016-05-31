module P36 where

toBase :: Int -> Int -> [Int]
toBase b n
	| n < b = [n]
	| otherwise = (toBase b $ n `div` b) ++ [n `mod` b]

isPalindrome arr = reverse arr == arr
isDecimalPalindrome n = isPalindrome $ show n
isBinaryPalindrome n = isPalindrome $ toBase 2 n

palindromes = [n | n <- [1..999999], isDecimalPalindrome n, isBinaryPalindrome n]
result = sum palindromes
