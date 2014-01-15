module P09 where
main = print result

result = head [a * b * c | a <- [1..1000], b <- [a..1000], let c = 1000-a-b, a*a + b*b == c*c]