module P54 where
import P51 (allEqual, groupAllBy)
import Data.List (sort, sortBy, find)
import Data.Ord (comparing)
import Data.Char (isDigit, digitToInt)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Enum, Eq, Ord)
data Card = Card { rank::Int, suit::Suit }

instance Show Suit where show Clubs = "C"; show Diamonds = "D"; show Hearts = "H"; show Spades = "S"
instance Show Card where show (Card r s) = (show r) ++ (show s)

main = do
	fileContents <- readFile "P54_poker.txt"
	let hands = parseFile fileContents
	let results = map compareHands hands
	print $ length $ filter (== GT) results

parseFile :: String -> [([Card], [Card])]
parseFile str = map parseLine $ lines str

parseLine :: String -> ([Card], [Card])
parseLine str = let cards = map parseCard $ words str
	in (take 5 cards, drop 5 cards)

parseCard :: String -> Card
parseCard str = Card (r $ str !! 0) (s $ str !! 1)
	where
		r 'T' = 10; r 'J' = 11; r 'Q' = 12; r 'K' = 13; r 'A' = 14
		r c | isDigit c = digitToInt c
		s 'C' = Clubs; s 'D' = Diamonds; s 'H' = Hearts; s 'S' = Spades

areConsecutive :: (Enum a, Eq a) => [a] -> Bool
areConsecutive [] = True
areConsecutive (_:[]) = True
areConsecutive (a:b:cs) = (b == succ a) && areConsecutive (b:cs)

sameSuit :: [Card] -> Bool
sameSuit cards = allEqual $ map suit cards

hasRanks :: [Card] -> [Int] -> Bool
hasRanks cards ranks = (sort $ map rank cards) == (sort ranks)

areConsecutiveRanks :: [Card] -> Bool
areConsecutiveRanks cards = areConsecutive $ sort $ map rank cards

highestRank :: [Card] -> Int
highestRank cards = maximum $ map rank cards

groupCards :: [Card] -> [(Int, [Card])]
groupCards cards = sortBy (comparing (length . snd)) (groupAllBy rank cards)

royalFlush hand = if (sameSuit hand) && (hasRanks hand [10,11,12,13,14]) then Just [1] else Nothing
straightFlush hand = if (sameSuit hand) && (areConsecutiveRanks hand) then Just [highestRank hand] else Nothing
fourOfAKind hand = (find ((== 4) . length . snd) (groupAllBy rank hand)) >>= \g -> Just [fst g]
fullHouse hand = let groups = groupCards hand in
	if [2,3] == map (length . snd) groups then Just [maximum $ map fst groups] else Nothing
flush hand = if sameSuit hand then Just [highestRank hand] else Nothing
straight hand = if areConsecutiveRanks hand then Just [highestRank hand] else Nothing
threeOfAKind hand = (find ((== 3) . length . snd) (groupAllBy rank hand)) >>= \g -> Just [fst g]
twoPairs hand = let groups = groupCards hand in
	if [1,2,2] == map (length . snd) groups
		then
			let pairs = drop 1 groups; single = head groups
			in Just ((reverse $ sort $ map fst $ pairs) ++ [fst single])
		else Nothing
onePair hand = let groups = groupCards hand in
	if [1,1,1,2] == map (length . snd) groups
		then
			let pair = groups !! 3; singles = take 3 groups
			in Just (fst pair : (reverse $ sort $ map fst $ singles))
		else Nothing
highCard hand = Just (reverse $ sort $ map rank hand)

handComparisons :: [[Card] -> Maybe [Int]]
handComparisons = [royalFlush, straightFlush, fourOfAKind, fullHouse, flush, straight, threeOfAKind, twoPairs, onePair, highCard]

compareHands :: ([Card],[Card]) -> Ordering
compareHands (a,b) = compareHands' a b handComparisons
	where
		compareHands' a b (c:[]) = compare (c a) (c b)
		compareHands' a b (c:cs) =
			let result = compare (c a) (c b) in
				if result == EQ then compareHands' a b cs else result