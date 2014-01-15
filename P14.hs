module P14 where
main = print result

collatz :: Int -> Int
collatz = (map collatz' [0 ..] !!) where
	collatz' 1 = 1
	collatz' n =
		if even n
			then 1+collatz (n `div` 2)
			else 1+collatz (3*n+1)

result = maximum (map collatz [1..1000000])