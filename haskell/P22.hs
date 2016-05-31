module P22 where
import Data.Char
import Data.List
import qualified Data.Text as T
main = do
	names <- readFile "P22_names.txt"
	print $ scoreNames names

removeQuotes :: T.Text -> T.Text
removeQuotes str = T.dropAround (== '"') str

parse :: String -> [String]
parse names = map (T.unpack . removeQuotes) $ T.splitOn (T.pack ",") (T.pack names)

scoreLetter :: Char -> Int
scoreLetter letter = ord (toUpper letter) - ord 'A' + 1

scoreLetters :: String -> Int
scoreLetters name = sum $ map scoreLetter name

scoreName :: String -> Int -> Int
scoreName name index = index * (scoreLetters name)

scoreNames :: String -> Int
scoreNames names = sum $ zipWith scoreName (sort $ parse names) [1..]
