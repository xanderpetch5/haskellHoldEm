import System.Random

-- Game Paramaters

defaultChips :: Int
defaultChips = 1000

-- NON SPECIFIC FUNCTIONS

randRange :: Int -> Int -> IO Int
randRange a b = randomRIO (a,b)

-- DEFINING CARDS AND SHUFFLING THE DECK

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq)

instance Show Card where
    show (Card {rank = rank, suit = suit}) = show rank ++ " of " ++ show suit


getRankValue :: Rank -> Int
getRankValue rank
    | rank == Two = 2
    | rank == Three = 3
    | rank == Four = 4
    | rank == Five = 5
    | rank == Six = 6
    | rank == Seven = 7
    | rank == Eight = 8
    | rank == Nine = 9
    | rank == Ten = 10
    | rank == Jack = 11
    | rank == Queen = 12
    | rank == King = 13
    | rank == Ace = 14

getSuitValue :: Suit -> Int
getSuitValue suit
    |suit == Clubs = 1
    |suit == Diamonds = 2
    |suit == Hearts = 3
    |suit == Spades = 4

getCardValue :: Card -> Int
getCardValue (Card rank suit) = 
    getRankValue rank

getSuits :: [Suit]
getSuits = [Clubs .. Spades]

getRanks :: [Rank]
getRanks = [Two .. Ace]

getDeck :: [Card]
getDeck = [Card rank suit | suit <- getSuits, rank <- getRanks]

shuffleDeck :: IO [Card]
shuffleDeck = shuffle getDeck
  where
    shuffle :: [a] -> IO [a]
    shuffle [] = return []
    shuffle xs = do
        randIndex <- randRange 0 (length xs - 1)
        let pickedElement = xs !! randIndex
            remainingElements = take randIndex xs ++ drop (randIndex + 1) xs
        rest <- shuffle remainingElements
        return (pickedElement : rest)
    
-- DEFINING PLAYER

data Action = Check | Raise | Call | Fold | Null
    deriving (Show)

data Player = Player{
    name :: String,
    hand :: [Card],
    chips :: Int,
    isPlaying :: Bool,
    action :: Action,
    isDealer :: Bool
}

instance Show Player where
    show (Player { name = n, hand = c, chips = ch, isPlaying = p, action = a , isDealer = i}) =
        "Player: " ++ n ++ "\n" ++
        "Chips: " ++ show ch ++ "\n" ++
        "Is Playing: " ++ show p ++ "\n" ++
        "Action: " ++ show a ++ "\n" ++
        "Cards: " ++ show c ++ "\n" ++
        "Dealer: " ++ show i

customPlayer:: String -> Bool -> Player
customPlayer name dealer = Player{
    name = name,
    hand = [],
    chips = defaultChips,
    isPlaying = True,
    action = Null,
    isDealer = dealer
}