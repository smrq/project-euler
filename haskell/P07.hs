module P07 where
main = print result

square x = x*x
primes = 2:3:sieve (tail primes) [5,7..] where
    sieve (lastKnownPrime:ps) primeCandidates = filteredCandidates ++ sieve ps [x | x <- remainingPrimeCandidates, rem x lastKnownPrime /= 0]
        where (filteredCandidates, ~(_:remainingPrimeCandidates)) = span (< square lastKnownPrime) primeCandidates

result = primes !! 10000
