module P10 where
main = print result

-- http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

primes = 2:([3..] `minus` composites) where composites = union [multiples p | p <- primes]

multiples n = map (n*) [n..]

(x:xs) `minus` (y:ys)
    | x < y = x:(xs `minus` (y:ys))
    | x == y = xs `minus` ys
    | x > y = (x:xs) `minus` ys

union = foldr merge []
    where
        merge (x:xs) ys = x:merge' xs ys
        merge' (x:xs) (y:ys)
            | x < y  = x:merge' xs (y:ys)
            | x == y = x:merge' xs ys
            | x > y  = y:merge' (x:xs) ys

primesBelowTwoMillion = takeWhile (< 2000000) primes
result = sum primesBelowTwoMillion