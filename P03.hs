module P03 where
main = print result

number = 600851475143

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n 2
    where
        primeFactors' 1 _ = []
        primeFactors' n divisor
            | n `mod` divisor == 0 = divisor : primeFactors' (n `div` divisor) divisor
            | otherwise            = primeFactors' n (divisor + 1)

result = maximum (primeFactors number)
