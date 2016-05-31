module P57 where

nextExpansion (num, denom) = (num + 2*denom, num + denom)
expansions = iterate nextExpansion (1, 1)

result = length $ filter match $ take 1000 expansions
	where match (num, denom) = (length $ show num) > (length $ show denom)
