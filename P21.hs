module P21 where
import Data.List
main = print result

--divisors x
--	| x <= 10000 = (map divisors' [0..10000] !! x)
--	| otherwise  = divisors' x
--		where divisors' x = [d | d <- [1..x-1], x `mod` d == 0]

divisors :: Int -> [Int]
divisors x = (properDivisors x) ++ [x]

properDivisors :: Int -> [Int]
properDivisors 0 = []
properDivisors 1 = []
properDivisors x = 1:(divisors' x 2)
	where
		divisors' :: Int -> Int -> [Int]
		divisors' x current
			| current*current == x	= [current]
			| current*current >  x	= []
			| x `rem` current == 0	= (current:(divisors' x (current+1))) ++ [x `div` current]
			| otherwise           	= divisors' x (current+1)

isAmicable a =
	let b = sum $ properDivisors a
	in (a /= b) && (a == (sum $ properDivisors b))

amicables = [x | x <- [1..9999], isAmicable x]
result = sum amicables