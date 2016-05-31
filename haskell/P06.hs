module P06 where
main = print result

square x = x * x
result = square (sum [1..100]) - sum (map square [1..100])