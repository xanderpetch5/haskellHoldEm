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

genericPlayerName :: Int -> String
genericPlayerName playerNum

    | playerNum == 1  = "Andrew"
    | playerNum == 2  = "Brian"
    | playerNum == 3  = "Charlie"
    | playerNum == 4  = "David"
    | playerNum == 5  = "Edward"
    | playerNum == 6  = "Frank"
    | playerNum == 7  = "George"
    | playerNum == 8  = "Henry"
    | playerNum == 9  = "Ian"
    | playerNum == 10 = "John"
    | otherwise       = "TOO MANY"

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
    bettingRound   :: BettingRound,
    revealedCommunityCards :: [Card]
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
        "Revealed Cards" ++ show (revealedCommunityCards gs) ++ "\n" ++
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

setBettingRound :: GameState -> BettingRound -> GameState
setBettingRound gameState newBettingRound = gameState {bettingRound = newBettingRound}

getNextBettingRound :: BettingRound -> BettingRound
getNextBettingRound currentRound
    | currentRound == PreFlop = Flop
    | currentRound == Flop = Turn
    | currentRound == Turn = River
    | currentRound == River = PreFlop

getNextRevealedCards :: [Card] -> GameState -> [Card]
getNextRevealedCards currentRevealed gameState
    | length currentRevealed == 0 = take 3 (communityCards gameState)
    | length currentRevealed == 3 = take 4 (communityCards gameState)
    | length currentRevealed == 4 = take 5 (communityCards gameState)
    | otherwise = []


nextTurn :: GameState -> GameState
nextTurn currentGameState =
    currentGameState {
        bets = [],
        bettingRound = getNextBettingRound (bettingRound currentGameState),
        revealedCommunityCards = getNextRevealedCards (revealedCommunityCards currentGameState) currentGameState
    }



customGameState :: [Player] -> [Card] -> Int -> GameState
customGameState players shuffledDeck dealerPos = GameState {
    players        = players,
    activePlayers  = players,
    deck           = shuffledDeck,
    communityCards = [],
    pot            = 0,
    bets           = [],
    dealerPos      = dealerPos,
    bettingRound   = PreFlop,
    revealedCommunityCards = []
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

-- HAND EVALUATION



data HandRanking
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush
    | RoyalFlush
    deriving (Show, Eq, Ord)

sortHandRankAndSuit :: [Card] -> [Card]
sortHandRankAndSuit = inSort
  where
    inSort :: [Card] -> [Card]
    inSort [] = []
    inSort (c:cs) = insert c (inSort cs)

    insert :: Card -> [Card] -> [Card]
    insert card [] = [card]
    insert card (x:xs)
        | cardValues card x = card : x : xs
        | otherwise          = x : insert card xs

    cardValues :: Card -> Card -> Bool
    cardValues a b =
        let aRank = getRankValue (rank a)
            bRank = getRankValue (rank b)
            aSuit = getSuitValue (suit a)
            bSuit = getSuitValue (suit b)
        in if aRank > bRank
           then True
           else if aRank == bRank
                then aSuit > bSuit
                else False




isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards
    |isStraightFlush cards && getCardValue (cards!!0) == 14 = True
    |otherwise = False

isStraightFlush :: [Card] -> Bool
isStraightFlush cards
    |isFlush cards && isStraight cards= True
    |otherwise = False

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards
    |values!!0 == values!!3 = True
    |values!!1 == values!!4 = True
    |otherwise = False
    where
        values = map getCardValue cards 

isFullHouse :: [Card] -> Bool
isFullHouse cards -- AABBB AAABB
    |values!!0 == values!!2 && values!!3 == values!!4 = True
    |values!!0 == values!!1 && values!!2 == values!!4 = True
    |otherwise = False
    where
        values = map getCardValue cards 

isFlush :: [Card] -> Bool
isFlush cards
    | length cards < 5 = False
    | otherwise = all (== firstSuit) suits
    where
        suits = map suit cards
        firstSuit = head suits

isStraight :: [Card] -> Bool
isStraight cards
    | adjustedValues == check = True
    | otherwise = False
    where
        values = map getCardValue cards
        minValue = last values
        adjustedValues = map (\x -> x - minValue) values 
        check = [4,3,2,1,0]

isThreeOfAKind :: [Card] -> Bool -- AAABB ABBBC ABCCC
isThreeOfAKind cards
    |values!!0 == values!!2 = True
    |values!!1 == values!!3 = True
    |values!!2 == values!!3 = True
    |otherwise = False
    where
        values = map getCardValue cards 

isTwoPair :: [Card] -> Bool -- AABBC AABCC ABBCC
isTwoPair cards
    |values!!0 == values!!1 && values!!2 == values!!3 = True
    |values!!0 == values!!1 && values!!3 == values!!4 = True
    |values!!1 == values!!2 && values!!3 == values!!4 = True
    |otherwise = False
    where
        values = map getCardValue cards 

isPair :: [Card] -> Bool --ABCDD ABCCD ABBCD AABCD
isPair cards
    |values!!0 == values!!1 = True
    |values!!1 == values!!2 = True
    |values!!2 == values!!3 = True
    |values!!3 == values!!4 = True
    |otherwise = False
    where
        values = map getCardValue cards 

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

handRanking :: [Card] -> HandRanking
handRanking cards
    | isRoyalFlush cards     = RoyalFlush
    | isStraightFlush cards  = StraightFlush
    | isFourOfAKind cards    = FourOfAKind
    | isFullHouse cards      = FullHouse
    | isFlush cards          = Flush
    | isStraight cards       = Straight
    | isThreeOfAKind cards   = ThreeOfAKind
    | isTwoPair cards        = TwoPair
    | isPair cards           = OnePair
    | otherwise              = HighCard

bestHand :: [[Card]] -> ([Card], HandRanking)
bestHand combos =
    let rankedHands = [(c, handRanking c) | c <- combos]
        sortedByRank = reverse (sortForRank rankedHands)
    in head sortedByRank

sortForRank :: [([Card], HandRanking)] -> [([Card], HandRanking)]
sortForRank [] = []
sortForRank (p:xs) =
    let lessOrEqual = [x | x <- xs, snd x <= snd p]
        greater     = [x | x <- xs, snd x > snd p]
    in sortForRank lessOrEqual ++ [p] ++ sortForRank greater

evaluateHand :: [Card] -> [Card] -> ([Card], HandRanking)
evaluateHand playerHand communityCards =
    let allCards = playerHand ++ communityCards
        combos = choose 5 allCards
        (bestCombo, bestRank) = bestHand combos
    in (bestCombo, bestRank)



-- IO

main :: IO ()
main = do
    let playerHand = [Card Ace Hearts, Card King Hearts]              -- Player's hole cards
    let community = [Card Queen Hearts, Card Jack Hearts, Card Ten Hearts, Card Nine Hearts, Card Eight Hearts]
    let (bestCombo, bestRank) = evaluateHand playerHand community
    putStrLn "Best 5-card hand:"
    print bestCombo
    putStrLn $ "Ranking: " ++ show bestRank
