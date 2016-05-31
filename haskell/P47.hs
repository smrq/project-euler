module P47 where
import P03 (primeFactors)
import Data.Char
import Data.List
import qualified Data.Text.Lazy as T

primeFactorCount = length . nub . primeFactors
primeFactorCountString = T.pack $ map (intToDigit . primeFactorCount) [1..]
result = 1 + (T.length $ fst $ T.breakOn (T.pack "4444") primeFactorCountString)