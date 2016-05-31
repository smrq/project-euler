module P29 where
import P10 (mergeAllSorted)

main = print result
result = length $ mergeAllSorted [[a^b | b <- [2..100]] | a <- [2..100]]