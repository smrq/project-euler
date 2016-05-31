module P01 where
main = print result

threeOrFive x = (x `mod` 3 == 0) || (x `mod` 5 == 0)
result = sum (filter threeOrFive [1..999])
