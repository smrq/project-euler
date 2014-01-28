module P55 where
import P36 (isDecimalPalindrome)
import Control.Monad.Trans.Cont (runCont, callCC)

stepsToPalindrome :: Integer -> Maybe Int
stepsToPalindrome n = (flip runCont) id $ do
	(k, n', ct) <- callCC $ \k -> let k' n ct = k (k', n, ct) in return (k', nextN n, 1)
	if isDecimalPalindrome n' then return (Just ct)
	else if ct >= 50 then return Nothing
	else (k (nextN n') (succ ct)) >>= return
	where nextN n = n + (read $ reverse $ show n)

isLychrel = (== Nothing) . stepsToPalindrome
result = length $ filter isLychrel [1..9999]