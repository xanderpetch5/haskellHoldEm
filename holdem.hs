import System.Random

-- Game Parameters

defaultChips :: Int
defaultChips = 1000

smallBlind :: Int
smallBlind = 50

bigBlind :: Int
bigBlind = 100

-- NON SPECIFIC FUNCTIONS

randRange :: Int -> Int -> IO Int
randRange a b = randomRIO (a, b)

-- DEFINING CARDS AND SHUFFLING THE DECK

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq)

instance Show Card where
    show (Card {rank = r, suit = s}) = show r ++ " of " ++ show s

getRankValue :: Rank -> Int
getRankValue rank
    | rank == Two   = 2
    | rank == Three = 3
    | rank == Four  = 4
    | rank == Five  = 5
    | rank == Six   = 6
    | rank == Seven = 7
    | rank == Eight = 8
    | rank == Nine  = 9
    | rank == Ten   = 10
    | rank == Jack  = 11
    | rank == Queen = 12
    | rank == King  = 13
    | rank == Ace   = 14

getSuitValue :: Suit -> Int
getSuitValue suit
    | suit == Clubs    = 1
    | suit == Diamonds = 2
    | suit == Hearts   = 3
    | suit == Spades   = 4

getCardValue :: Card -> Int
getCardValue (Card rank _) = getRankValue rank

getSuits :: [Suit]
getSuits = [Clubs .. Spades]

getRanks :: [Rank]
getRanks = [Two .. Ace]

getDeck :: [Card]
getDeck = [Card rank suit | suit <- getSuits, rank <- getRanks]

shuffleDeck :: [Card] -> IO [Card]
shuffleDeck [] = return []
shuffleDeck xs = do
    randIndex <- randRange 0 (length xs - 1)
    let (pickedElement, remainingElements) = removeAt randIndex xs
    rest <- shuffleDeck remainingElements
    return (pickedElement : rest)
  where
    removeAt :: Int -> [a] -> (a, [a])
    removeAt i xs = (xs !! i, take i xs ++ drop (i + 1) xs)

-- DEFINING PLAYER

data Action = Check | Raise | Call | Fold | Null
    deriving (Show, Eq)

data Strategy = Random | Aggressive
    deriving (Show, Eq)

data Player = Player {
    name      :: String,
    hand      :: [Card],
    chips     :: Int,
    isPlaying :: Bool,
    action    :: Action,
    isDealer  :: Bool,
    strategy  :: Strategy
} deriving (Eq)

-- Setter functions for Player

setIsPlaying :: Player -> Bool -> Player
setIsPlaying player newIsPlaying = player { isPlaying = newIsPlaying }

setAction :: Player -> Action -> Player
setAction player newAction = player { action = newAction }

setHand :: Player -> [Card] -> Player
setHand player newHand = player { hand = newHand }

setChips :: Player -> Int -> Player
setChips player newChips = player { chips = newChips }

setIsDealer :: Player -> Bool -> Player
setIsDealer player newIsDealer = player { isDealer = newIsDealer }

instance Show Player where 
    show (Player { name = n, hand = c, chips = ch, isPlaying = p, action = a, isDealer = i, strategy = s }) =
        "Player: " ++ n ++ "\n" ++
        "Chips: " ++ show ch ++ "\n" ++
        "Is Playing: " ++ show p ++ "\n" ++
        "Action: " ++ show a ++ "\n" ++
        "Cards: " ++ show c ++ "\n" ++
        "Dealer: " ++ show i ++ "\n" ++
        "Strategy: " ++ show s

customPlayer :: String -> Bool -> Player
customPlayer playerName dealer = Player {
    name      = playerName,
    hand      = [],
    chips     = defaultChips,
    isPlaying = True,
    action    = Null,
    isDealer  = dealer,
    strategy  = Random
}



-- Defining GameState

data BettingRound = PreFlop | Flop | Turn | River
    deriving (Eq, Show)

data GameState = GameState {
    players        :: [Player],
    activePlayers  :: [Player],
    deck           :: [Card],
    communityCards :: [Card],
    pot            :: Int,
    bets           :: [Int],
    dealerPos      :: Int,
    bettingRound   :: BettingRound
} deriving (Eq)

instance Show GameState where
    show gs =
        "=== GameState ===\n" ++
        "Number of Players: " ++ show (length $ players gs) ++ "\n" ++
        "Players:\n" ++ unlines (map show (players gs)) ++
        "Active Players:\n" ++ unlines (map show (activePlayers gs)) ++
        "Community Cards: " ++ show (communityCards gs) ++ "\n" ++
        "Pot: " ++ show (pot gs) ++ "\n" ++
        "Bets: " ++ show (bets gs) ++ "\n" ++
        "Dealer Position: " ++ show (dealerPos gs) ++ "\n" ++
        "Betting Round: " ++ show (bettingRound gs) ++ "\n" ++
        "Remaining Deck Size: " ++ show (length $ deck gs) ++ "\n" ++
        "=================="



-- Setter functions for GameState

setActivePlayers :: GameState -> [Player] -> GameState
setActivePlayers gameState newActivePlayers = gameState { activePlayers = newActivePlayers }

setDeck :: GameState -> [Card] -> GameState
setDeck gameState newDeck = gameState { deck = newDeck }

setCommunityCards :: GameState -> [Card] -> GameState
setCommunityCards gameState newCommunityCards = gameState { communityCards = newCommunityCards }

setPot :: GameState -> Int -> GameState
setPot gameState newPot = gameState { pot = newPot }

setBets :: GameState -> [Int] -> GameState
setBets gameState newBets = gameState { bets = newBets }

setDealerPos :: GameState -> Int -> GameState
setDealerPos gameState newDealerPos = gameState { dealerPos = newDealerPos }


customGameState :: [Player] -> [Card] -> Int -> GameState
customGameState players shuffledDeck dealerPos = GameState {
    players        = players,
    activePlayers  = players,
    deck           = shuffledDeck,
    communityCards = [],
    pot            = 0,
    bets           = [],
    dealerPos      = dealerPos,
    bettingRound   = PreFlop
}


-- Deal cards

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

replacePlayers :: [Player] -> [Player] -> [Player]
replacePlayers orig active =
    active ++ drop (length active) orig

dealCards :: GameState -> GameState
dealCards gameState =
    let
        activePs = activePlayers gameState
        numPlayers = length activePs

        cardsToDealToPlayers = numPlayers * 2
        (playerCards, deckAfterPlayers) = splitAt cardsToDealToPlayers (deck gameState)

        (community, remainingDeck) = splitAt 5 deckAfterPlayers

        cardPairs = chunksOf 2 playerCards

        updatedPlayers = zipWith (\player cards -> player { hand = cards }) activePs cardPairs

        allPlayers = players gameState
        updatedAllPlayers = replacePlayers (players gameState) updatedPlayers
    in
        gameState {
            players = updatedAllPlayers,
            activePlayers = updatedPlayers,
            deck = remainingDeck,
            communityCards = community
        }



-- IO

main :: IO ()
main = do
    let player1 = customPlayer "Alice" True
    let player2 = customPlayer "Bob" False
    let playersList = [player1, player2]
    
    shuffledDeck <- shuffleDeck getDeck
    let initialGameState = customGameState playersList shuffledDeck 0
    let finalGameState = dealCards initialGameState
    
    print finalGameState
