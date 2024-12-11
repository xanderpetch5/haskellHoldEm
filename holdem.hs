data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Enum, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Enum, Bounded)
data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Show, Eq)

instance Show Card where
    show (Card r s) = show r ++ " of " ++ show s

getSuits :: [Suit]
getSuits = [Clubs .. Spades]

getRanks :: [Rank]
getRanks = [Two .. Ace]

getDeck :: [Card]
getDeck = [Card rank suit | suit <- getSuits, rank <- getRanks]

main :: IO ()
main = do
    print getDeck
