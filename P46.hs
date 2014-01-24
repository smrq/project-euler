module P46 where
import P10 (oddComposites)
import P27 (isPrime)

failsConjecture composite = not $ any isPrime $ map (\x -> composite - 2*(x*x)) [1..floor $ sqrt (fromIntegral composite)]
result = head $ filter failsConjecture oddComposites