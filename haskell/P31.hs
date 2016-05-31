module P31 where
main = print result

coinCombinations 0 _ = 1
coinCombinations target [coin] = if target `mod` coin == 0 then 1 else 0
coinCombinations target values =
	sum [coinCombinations newTarget newValues |
		coin <- values,
		let newTarget = target - coin,
		let newValues = filter (<= coin) values,
		newTarget >= 0]

result = coinCombinations 200 [200,100,50,20,10,5,2,1]