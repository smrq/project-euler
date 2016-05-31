module P59 where
import Data.Bits (xor)
import Data.Array.IArray (Array, accumArray, assocs)
import Data.Ix (Ix, inRange)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (ord, chr, isAsciiLower)
import qualified Data.Text as T

main = result >>= print

result :: IO Int
result = do
	plaintext <- decodeWithGuess (0,1,0) -- Guess determined manually via decodeWithGuess
	return $ sum $ map ord plaintext

decodeWithGuess :: (Int, Int, Int) -> IO String
decodeWithGuess (i, i', i'') = do
	fileContents <- readFile "P59_cipher.txt"
	let cipher = parseFile fileContents
	let (c, c', c'') = groupBy3 cipher
	let key =
		[ cipherCandidates c !! i
		, cipherCandidates c' !! i'
		, cipherCandidates c'' !! i'' ]
	return $ decode cipher key

cipherCandidates :: [Int] -> [Char]
cipherCandidates ls = filter isAsciiLower $ map (chr . (`xor` (ord 'e'))) $ frequentValues ls

parseFile :: String -> [Int]
parseFile str = map (read . T.unpack) $ T.splitOn (T.pack ",") (T.pack str)

decode :: [Int] -> String -> String
decode [] _ = []
decode cipher key = (chr ((head cipher) `xor` (ord $ head key))) : (decode (tail cipher) ((tail key) ++ [head key]))

groupBy3 :: [a] -> ([a],[a],[a])
groupBy3 [] = ([],[],[])
groupBy3 (x:[]) = ([x],[],[])
groupBy3 (x:y:[]) = ([x],[y],[])
groupBy3 (x:y:z:rest) = let (x',y',z') = groupBy3 rest in (x:x',y:y',z:z')

frequentValues :: [Int] -> [Int]
frequentValues ls = map fst $ sortBy (flip $ comparing snd) $ assocs $ hist (0,127) ls

hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]