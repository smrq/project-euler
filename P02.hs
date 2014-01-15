module P02 where
main = print result

--Attempt 1:
--fib :: Int -> Integer
--fib = (map rFib [0 ..] !!)
--    where rFib 0 = 0
--          rFib 1 = 1
--          rFib n = fib (n-2) + fib (n-1)
--fibs = map fib [0..]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
result = sum (filter even (takeWhile (< 4000000) fibs))