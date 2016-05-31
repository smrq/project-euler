module P25 where
import P02 (fibs)
main = print result

result = snd $ head $ filter ((== 1000) . length . show . fst) (zip fibs [1..])