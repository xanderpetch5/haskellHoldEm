import System.Random

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Show, Eq)


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

getSuits :: [Suit]
getSuits = [Clubs .. Spades]

getRanks :: [Rank]
getRanks = [Two .. Ace]

getDeck :: [Card]
getDeck = [Card rank suit | suit <- getSuits, rank <- getRanks]

randRange :: Int -> Int -> IO Int
randRange a b = randomRIO (a,b)

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
    
