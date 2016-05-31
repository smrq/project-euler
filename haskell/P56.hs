module P56 where
import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (comparing)

digitalSum n = sum $ map digitToInt $ show n
result = maximumBy (comparing (\(_,_,s) -> s)) $ [(a, b, digitalSum (a^b)) | a <- [1..100], b <- [1..100]]