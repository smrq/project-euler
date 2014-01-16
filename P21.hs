module P21 where
main = print result

divisors x
	| x <= 10000 = (map divisors' [0..10000] !! x)
	| otherwise  = divisors' x
		where divisors' x = [d | d <- [1..x-1], x `mod` d == 0]

isAmicable a =
	let b = sum $ divisors a
	in (a /= b) && (a == (sum $ divisors b))

amicables = [x | x <- [1..9999], isAmicable x]
result = sum amicables