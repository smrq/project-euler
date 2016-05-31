module P71 where
import Data.Ratio (numerator, (%))

numeratorLeftOfThreeSevenths denom = floor $ (fromIntegral denom) * 3/7
result = maximum $ filter (/= (3%7)) $ map (\n -> (numeratorLeftOfThreeSevenths n) % n) [1..1000000]
