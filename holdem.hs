{-# LANGUAGE ScopedTypeVariables #-}

import System.Random

-- | Default chip count assigned to each player at the start
defaultChips :: Int
defaultChips = 1000

-- | Small and big blind amounts
smallBlindAmount :: Int
smallBlindAmount = 50

bigBlindAmount :: Int
bigBlindAmount = 100

-- | Custom intercalate function to join a list of strings with a separator
intercalateCustom :: String -> [String] -> String
intercalateCustom _ [] = ""
intercalateCustom _ [x] = x
intercalateCustom sep (x:xs) = x ++ sep ++ intercalateCustom sep xs

-- | Custom sort function for a list of Cards based on their values
sortCards :: [Card] -> [Card]
sortCards [] = []
sortCards (card:cards) = insertCard card (sortCards cards)
  where
    insertCard :: Card -> [Card] -> [Card]
    insertCard c [] = [c]
    insertCard c (y:ys)
        | getCardValue c <= getCardValue y = c : y : ys
        | otherwise = y : insertCard c ys

-- | Custom sort function for a list of Integers
sortInts :: [Int] -> [Int]
sortInts [] = []
sortInts (x:xs) = insertInt x (sortInts xs)
  where
    insertInt :: Int -> [Int] -> [Int]
    insertInt n [] = [n]
    insertInt n (y:ys)
        | n <= y    = n : y : ys
        | otherwise = y : insertInt n ys

-- | Custom function to remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- | Custom function to find the index of an element in a list
findIndexCustom :: Eq a => a -> [a] -> Maybe Int
findIndexCustom _ [] = Nothing
findIndexCustom target (y:ys)
    | target == y = Just 0
    | otherwise = case findIndexCustom target ys of
                    Just n  -> Just (n + 1)
                    Nothing -> Nothing

-- | Set an element at a specific index in a list
setElementAt :: Int -> a -> [a] -> [a]
setElementAt _ _ [] = []
setElementAt 0 newVal (_:xs) = newVal : xs
setElementAt i newVal (x:xs) = x : setElementAt (i - 1) newVal xs

-- | Replace a player in the players list at a specific index
replacePlayerAt :: [Player] -> Int -> Player -> [Player]
replacePlayerAt playersList index newPlayer = take index playersList ++ [newPlayer] ++ drop (index + 1) playersList

-- | Remove an element at a specific index from a list, returning the element and the new list
removeAtIndex :: Int -> [a] -> (a, [a])
removeAtIndex i xs = (xs !! i, take i xs ++ drop (i + 1) xs)

-- | Generate a random number within a specified range
randomInRange :: Int -> Int -> IO Int
randomInRange lower upper = randomRIO (lower, upper)

-- | Custom sortBy function that sorts a list based on a comparison function
sortByCustom :: (a -> a -> Ordering) -> [a] -> [a]
sortByCustom _ [] = []
sortByCustom cmp (x:xs) = insertBy cmp x (sortByCustom cmp xs)
  where
    insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
    insertBy _ y [] = [y]
    insertBy c y (z:zs) =
        case c y z of
            GT -> z : insertBy c y zs
            _  -> y : z : zs

-- | Suits in a standard deck of cards
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq, Enum, Bounded)

-- | Ranks in a standard deck of cards
data Rank
    = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Bounded)

-- | Card data type representing a playing card with rank and suit
data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq)

instance Show Card where
    show (Card {rank = r, suit = s}) = show r ++ " of " ++ show s

-- | Get numerical value of a rank
getRankValue :: Rank -> Int
getRankValue rnk = case rnk of
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9
    Ten   -> 10
    Jack  -> 11
    Queen -> 12
    King  -> 13
    Ace   -> 14

-- | Get numerical value of a card based on its rank
getCardValue :: Card -> Int
getCardValue card = getRankValue (rank card)

-- | Generate a standard deck of 52 playing cards
generateDeck :: [Card]
generateDeck = [Card r s | s <- [Clubs .. Spades], r <- [Two .. Ace]]

-- | Shuffle the deck of cards recursively
shuffleDeck :: [Card] -> IO [Card]
shuffleDeck [] = return []
shuffleDeck deck = do
    randomIndex <- randomInRange 0 (length deck - 1)
    let (removedCard, remainingDeck) = removeAtIndex randomIndex deck
    shuffledRemaining <- shuffleDeck remainingDeck
    return (removedCard : shuffledRemaining)

-- | Hand ranking in poker
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

-- | Probability assigned to each hand ranking
handTypeProbability :: HandRanking -> Int
handTypeProbability handType = case handType of
    HighCard       -> 4
    OnePair        -> 14
    TwoPair        -> 33
    ThreeOfAKind   -> 50
    Straight       -> 60
    Flush          -> 70
    FullHouse      -> 90
    FourOfAKind    -> 97
    StraightFlush  -> 99
    RoyalFlush     -> 100

-- | Calculate hand value based on the current game phase
handValueCalculator :: GamePhase -> HandRanking -> Int
handValueCalculator phase handRank
    | phase == Flop  = handTypeProbability handRank + 50
    | phase == Turn  = handTypeProbability handRank + 35
    | phase == River = handTypeProbability handRank + 20
    | otherwise      = handTypeProbability handRank

-- | Game phases in poker
data GamePhase = PreFlop | Flop | Turn | River
    deriving (Eq, Show)

-- | Actions a player can take
data Action = Check | Raise | Call | Fold | NullAction
    deriving (Show, Eq)

-- | Player strategies
data Strategy = RandomStrategy | AggressiveStrategy | PassiveStrategy | SmartStrategy
    deriving (Show, Eq)

-- | Player data type representing a participant in the game
data Player = Player {
    name        :: String,
    hand        :: [Card],
    chips       :: Int,
    isPlaying   :: Bool,
    hasFolded   :: Bool,
    action      :: Action,
    isDealer    :: Bool,
    strategy    :: Strategy,
    playerIndex :: Int
} deriving (Eq)

instance Show Player where
    show (Player { name = playerName, hand = playerHand, chips = playerChips, isPlaying = playing,
                  hasFolded = folded, action = currentAction, isDealer = dealer,
                  strategy = playerStrategy, playerIndex = index}) =
        "Player #" ++ show index ++ " - " ++ playerName ++ ":\n" ++
        "  Chips        : " ++ show playerChips ++ "\n" ++
        "  Status       : " ++ (if playing then "Active" else "Out") ++ "\n" ++
        "  Folded       : " ++ (if folded then "Yes" else "No") ++ "\n" ++
        "  Action       : " ++ show currentAction ++ "\n" ++
        "  Cards        : " ++ showPlayerCards playerHand ++ "\n" ++
        "  Dealer       : " ++ (if dealer then "Yes" else "No") ++ "\n" ++
        "  Strategy     : " ++ show playerStrategy ++ "\n"
      where
        showPlayerCards :: [Card] -> String
        showPlayerCards []     = "None"
        showPlayerCards [card] = show card
        showPlayerCards cards  = intercalateCustom ", " (map show cards)

-- | Create a custom player with given attributes
createPlayer :: String -> Bool -> Int -> Strategy -> Player
createPlayer playerName dealer index strat = Player {
    name        = playerName,
    hand        = [],
    chips       = defaultChips,
    isPlaying   = True,
    hasFolded   = False,
    action      = NullAction,
    isDealer    = dealer,
    strategy    = strat,
    playerIndex = index
}

-- | Game state data type representing the current state of the game
data GameState = GameState {
    players                :: [Player],
    activePlayers          :: [Player],
    deck                   :: [Card],
    communityCards         :: [Card],
    pot                    :: Int,
    bets                   :: [Int],
    contributions          :: [Int],
    dealerPos              :: Int,
    gamePhase              :: GamePhase,
    revealedCommunityCards :: [Card],
    roundWinner            :: Maybe Player,
    roundWinnerHand        :: Maybe HandRanking,
    bigBlindIndex          :: Int,
    smallBlindIndex        :: Int
} deriving (Eq)

instance Show GameState where
    show gs =
        "=== GameState ===\n" ++
        "General Information:\n" ++
        "  Number of Players : " ++ show (length $ players gs) ++ "\n" ++
        "  Dealer Position   : " ++ show (dealerPos gs) ++ "\n" ++
        "  Game Phase        : " ++ show (gamePhase gs) ++ "\n" ++
        "  Pot               : " ++ show (pot gs) ++ "\n" ++
        "  Remaining Deck    : " ++ show (length $ deck gs) ++ " cards\n" ++
        "\nPlayers:\n" ++
        concatMap show (players gs) ++
        "\nActive Players:\n" ++
        (if null (activePlayers gs)
            then "  None\n"
            else concatMap show (activePlayers gs)) ++
        "\nCommunity Cards:\n  " ++ showCommunityCards (communityCards gs) ++ "\n" ++
        "\nBets:\n  " ++ show (bets gs) ++ "\n" ++
        "Contributions:\n  " ++ show (contributions gs) ++ "\n" ++
        "\nRound Winner:\n  " ++ showWinnerWithHand (roundWinner gs, roundWinnerHand gs) ++ "\n" ++
        "=================="
      where
        showCommunityCards :: [Card] -> String
        showCommunityCards []     = "None"
        showCommunityCards [card] = show card
        showCommunityCards cards  = intercalateCustom ", " (map show cards)

        showWinnerWithHand :: (Maybe Player, Maybe HandRanking) -> String
        showWinnerWithHand (Nothing, _) = "None"
        showWinnerWithHand (Just winner, Just handType) =
            name winner ++ " (Player #" ++ show (playerIndex winner) ++ ") - " ++ show handType
        showWinnerWithHand (Just winner, Nothing) =
            name winner ++ " (Player #" ++ show (playerIndex winner) ++ ")"

-- | Get a list of active players (playing and not folded), removing duplicates
getActivePlayers :: [Player] -> [Player]
getActivePlayers = removeDuplicates . filter (\p -> isPlaying p && not (hasFolded p))

-- | Get the highest current bet in the game
highestBet :: GameState -> Int
highestBet gs = if null (bets gs) then 0 else maximum (bets gs)

-- | Set the current game phase
setGamePhase :: GameState -> GamePhase -> GameState
setGamePhase gameState newPhase = gameState { gamePhase = newPhase }

-- | Eliminate players who have zero or fewer chips
eliminatePlayers :: GameState -> GameState
eliminatePlayers gs =
    let updatedPlayers = map eliminateIfBroke (players gs)
        eliminateIfBroke player
            | chips player <= 0 = player { isPlaying = False }
            | otherwise         = player
        updatedActive = getActivePlayers updatedPlayers
    in gs { players = updatedPlayers, activePlayers = updatedActive }
  where
    -- Additional helper functions can be defined here if necessary

-- | Post blinds to the pot
postBlinds :: GameState -> GameState
postBlinds gs =
    let sbPlayer = players gs !! (smallBlindIndex gs)
        bbPlayer = players gs !! (bigBlindIndex gs)
        sbAmount = min smallBlindAmount (chips sbPlayer)
        bbAmount = min bigBlindAmount (chips bbPlayer)
        sbPlayer' = sbPlayer { chips = chips sbPlayer - sbAmount }
        bbPlayer' = bbPlayer { chips = chips bbPlayer - bbAmount }
        updatedPlayers = replacePlayerAt (replacePlayerAt (players gs) (smallBlindIndex gs) sbPlayer') (bigBlindIndex gs) bbPlayer'
        updatedBets = bets gs
        updatedBets' = setElementAt (smallBlindIndex gs) sbAmount updatedBets
        updatedBets'' = setElementAt (bigBlindIndex gs) bbAmount updatedBets'
        updatedContributions = contributions gs
        updatedContributions' = setElementAt (smallBlindIndex gs) sbAmount (setElementAt (bigBlindIndex gs) bbAmount updatedContributions)
        updatedActivePlayers = getActivePlayers updatedPlayers
    in gs { players = updatedPlayers,
            activePlayers = updatedActivePlayers,
            bets = updatedBets'',
            contributions = updatedContributions' }

-- | Split a list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Deal cards to players and the community
dealCards :: GameState -> GameState
dealCards gs =
    let activePs = activePlayers gs
        numPlayers = length activePs
        cardsToDeal = numPlayers * 2
        (playerCards, remainingDeck) = splitAt cardsToDeal (deck gs)
        (community, finalDeck) = splitAt 5 remainingDeck
        cardPairs = chunksOf 2 playerCards
        updatedPlayers = zipWith (\player crds -> player { hand = crds }) activePs cardPairs
        allPlayers = replacePlayers (players gs) updatedPlayers
    in gs { players = allPlayers,
            activePlayers = getActivePlayers allPlayers,
            deck = finalDeck,
            communityCards = community }
  where
    replacePlayers :: [Player] -> [Player] -> [Player]
    replacePlayers original updated = foldl replace original updated
      where
        replace :: [Player] -> Player -> [Player]
        replace pls upd = replacePlayerAt pls (playerIndex upd) upd

-- | Determine the next game phase based on the current phase
getNextGamePhase :: GamePhase -> GamePhase
getNextGamePhase currentPhase
    | currentPhase == PreFlop = Flop
    | currentPhase == Flop    = Turn
    | currentPhase == Turn    = River
    | currentPhase == River   = PreFlop

-- | Get the next set of revealed community cards based on the game phase
getNextRevealedCards :: [Card] -> GameState -> [Card]
getNextRevealedCards currentRevealed gs
    | length currentRevealed == 0 = take 3 (communityCards gs)
    | length currentRevealed == 3 = take 4 (communityCards gs)
    | length currentRevealed == 4 = take 5 (communityCards gs)
    | otherwise = currentRevealed

-- | Advance the game phase and update revealed community cards
advanceGamePhase :: GameState -> GameState
advanceGamePhase gs =
    let newPhase = getNextGamePhase (gamePhase gs)
        newRevealed = getNextRevealedCards (revealedCommunityCards gs) gs
    in gs { gamePhase = newPhase, revealedCommunityCards = newRevealed }

-- | Handle a player folding
playerFolds :: GameState -> Int -> GameState
playerFolds gs foldedActiveIndex =
    let foldedPlayer = activePlayers gs !! foldedActiveIndex
        globalIndex = playerIndex foldedPlayer
        player = players gs !! globalIndex
        updatedPlayer = player { hasFolded = True, action = Fold }
        updatedPlayers = replacePlayerAt (players gs) globalIndex updatedPlayer
        gsAfterFold = gs { players = updatedPlayers }
    in eliminatePlayers gsAfterFold

-- | Handle a player calling
playerCalls :: GameState -> Int -> GameState
playerCalls gs pIndex =
    let highestB = highestBet gs
        currentBet = (bets gs) !! pIndex
        toCall = highestB - currentBet
        player = players gs !! pIndex
        actualCall = min toCall (chips player)
        updatedPlayer = player { chips = chips player - actualCall, action = Call }
        updatedPlayers = replacePlayerAt (players gs) pIndex updatedPlayer
        newBets = setElementAt pIndex highestB (bets gs)
        newContributions = contributions gs
        updatedContributions = setElementAt pIndex (contributions gs !! pIndex + actualCall) newContributions
        updatedActivePlayers = getActivePlayers updatedPlayers
    in eliminatePlayers gs { players = updatedPlayers, bets = newBets, contributions = updatedContributions, activePlayers = updatedActivePlayers }

-- | Handle a player checking
playerChecks :: GameState -> Int -> GameState
playerChecks gs pIndex =
    let highestB = highestBet gs
    in if highestB == 0
       then eliminatePlayers gs { players = markPlayerAction gs pIndex Check }
       else gs

-- | Handle a player raising
playerRaises :: GameState -> Int -> Int -> GameState
playerRaises gs pIndex raiseTo =
    let currentBet = (bets gs) !! pIndex
        toRaise = raiseTo - currentBet
        player = players gs !! pIndex
        actualRaise = min toRaise (chips player)
        updatedPlayer = player { chips = chips player - actualRaise, action = Raise }
        updatedPlayers = replacePlayerAt (players gs) pIndex updatedPlayer
        newBets = setElementAt pIndex raiseTo (bets gs)
        newContributions = contributions gs
        updatedContributions = setElementAt pIndex (contributions gs !! pIndex + actualRaise) newContributions
        updatedActivePlayers = getActivePlayers updatedPlayers
    in eliminatePlayers gs { players = updatedPlayers, bets = newBets, contributions = updatedContributions, activePlayers = updatedActivePlayers }

-- | Check if a hand is a pair
isPair :: [Card] -> Bool
isPair cards =
    let cardValues = getCardValues cards
        uniqueValues = removeDuplicates cardValues
        pairCount = length $ filter (\v -> length (filter (== v) cardValues) == 2) uniqueValues
    in pairCount >= 1

-- | Check if a hand is two pairs
isTwoPair :: [Card] -> Bool
isTwoPair cards =
    let cardValues = getCardValues cards
        uniqueValues = removeDuplicates cardValues
        pairCount = length $ filter (\v -> length (filter (== v) cardValues) == 2) uniqueValues
    in pairCount >= 2

-- | Check if a hand has three of a kind
isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards =
    let groups = groupCardValues cards
    in (length $ filter (== 3) groups) >= 1 && not (isFullHouse cards)

-- | Check if a hand is a straight
isStraight :: [Card] -> Bool
isStraight cards =
    let sortedValues = sortInts $ getCardValues cards
        distinctValues = removeDuplicates sortedValues
    in if length distinctValues < 5 then False
       else checkSequential distinctValues

-- | Check if any sequence of 5 values is sequential (straight)
checkSequential :: [Int] -> Bool
checkSequential vals
    | length vals < 5 = False
    | otherwise =
        let slidingWindows = takeSliding 5 vals
        in any isSequential slidingWindows

-- | Create sliding windows of size n from a list
takeSliding :: Int -> [a] -> [[a]]
takeSliding _ [] = []
takeSliding n xs
    | length xs < n = []
    | otherwise = take n xs : takeSliding n (tail xs)

-- | Check if a list of integers is sequential
isSequential :: [Int] -> Bool
isSequential xs = all (\(a, b) -> b == a + 1) (zip xs (tail xs))

-- | Check if a hand is a flush
isFlush :: [Card] -> Bool
isFlush cards
    | length cards < 5 = False
    | otherwise =
        let targetSuit = suit (head cards)
            sameSuitCount = length $ filter ((== targetSuit) . suit) cards
        in sameSuitCount >= 5

-- | Check if a hand is a straight flush
isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isFlush cards && isStraight cards

-- | Check if a hand is a royal flush
isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards =
    isStraightFlush cards &&
    sortInts (getCardValues cards) == [10, 11, 12, 13, 14]

-- | Check if a hand has four of a kind
isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = 4 `elem` groupCardValues cards

-- | Check if a hand is a full house
isFullHouse :: [Card] -> Bool
isFullHouse cards =
    let groups = groupCardValues cards
    in sortInts groups == [2, 3]

-- | Get the numerical values of the cards
getCardValues :: [Card] -> [Int]
getCardValues = map getCardValue

-- | Group and count the values in a hand
groupCardValues :: [Card] -> [Int]
groupCardValues cards =
    let vals = getCardValues cards
        counts = map (\v -> length (filter (== v) vals)) vals
    in sortInts counts

-- | Determine the ranking of a hand
determineHandRanking :: [Card] -> HandRanking
determineHandRanking cards
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

-- | Combine a list of card combinations with their rankings and select the best
bestHandCombo :: [([Card], HandRanking)] -> ([Card], HandRanking)
bestHandCombo combos =
    let sortedCombos = sortByCustom (\x y -> compare (snd x) (snd y)) combos
    in last sortedCombos

-- | Determine the best hand for a player based on their cards and community cards
evaluatePlayerHand :: [Card] -> [Card] -> ([Card], HandRanking)
evaluatePlayerHand playerHand communityCards =
    let allCards = playerHand ++ communityCards
        combinations = choose 5 allCards
        rankedCombinations = [(combo, determineHandRanking combo) | combo <- combinations]
    in if null rankedCombinations
       then ([], HighCard)
       else bestHandCombo rankedCombinations

-- | Choose n elements from a list
choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n - 1) xs) ++ choose n xs

-- | Evaluate the best hand when there are exactly five cards
evaluateBestHand5 :: [Card] -> ([Card], HandRanking)
evaluateBestHand5 hand
    | length hand /= 5 = error "evaluateBestHand5 requires exactly five cards."
    | otherwise        = (hand, determineHandRanking hand)

-- | Evaluate the best hand when there are exactly six cards
evaluateBestHand6 :: [Card] -> ([Card], HandRanking)
evaluateBestHand6 hand
    | length hand /= 6 = error "evaluateBestHand6 requires exactly six cards."
    | otherwise        = bestHandCombo [ (combo, determineHandRanking combo) | combo <- choose 5 hand ]

-- | Determine the best hand among all players
bestHandAmongPlayers :: [(Player, ([Card], HandRanking))] -> (Player, ([Card], HandRanking))
bestHandAmongPlayers playerHands =
    let sorted = sortByCustom (\x y -> compare (snd (snd x)) (snd (snd y))) playerHands
    in last sorted

-- | Determine the winner of the round
determineRoundWinner :: GameState -> GameState
determineRoundWinner gs =
    let commCards = communityCards gs
        activePs = activePlayers gs
        playerResults = [(p, evaluatePlayerHand (hand p) commCards) | p <- activePs]
    in if length activePs == 1
       then
           let winner = head activePs
               totalPot = sum (contributions gs)
               updatedWinner = winner { chips = chips winner + totalPot }
               updatedPlayers = replacePlayerAt (players gs) (playerIndex winner) updatedWinner
               gsAfterWinner = gs { players = updatedPlayers, pot = 0, roundWinner = Just updatedWinner, roundWinnerHand = Nothing }
               finalGS = eliminatePlayers gsAfterWinner
           in finalGS
       else if null playerResults
           then gs { roundWinner = Nothing }
           else
               let (winner, bestResult) = bestHandAmongPlayers playerResults
                   (_, bestRank) = bestResult
                   totalPot = sum (contributions gs)
                   updatedWinner = winner { chips = chips winner + totalPot }
                   updatedPlayers = replacePlayerAt (players gs) (playerIndex winner) updatedWinner
                   gsAfterWinner = gs { players = updatedPlayers, pot = 0, roundWinner = Just updatedWinner, roundWinnerHand = Just bestRank }
                   finalGS = eliminatePlayers gsAfterWinner
               in finalGS



-- | Calculate hand value based on game phase
calculateHandValue :: GamePhase -> HandRanking -> Int
calculateHandValue phase handRank
    | phase == Flop  = handTypeProbability handRank + 50
    | phase == Turn  = handTypeProbability handRank + 35
    | phase == River = handTypeProbability handRank + 20
    | otherwise      = handTypeProbability handRank

-- | Assign action based on player's strategy
performStrategyAction :: GameState -> Int -> IO GameState
performStrategyAction gs activePlayerIdx = do
    let player = activePlayers gs !! activePlayerIdx
    case strategy player of
        RandomStrategy     -> randomAction gs activePlayerIdx
        AggressiveStrategy -> aggressiveAction gs activePlayerIdx
        PassiveStrategy    -> passiveAction gs activePlayerIdx
        SmartStrategy      -> smartAction gs activePlayerIdx

-- | Determine the starting player index based on the game phase
getStartingPlayerIndex :: GameState -> Int
getStartingPlayerIndex gs =
    case gamePhase gs of
        PreFlop ->
            let pCount = length (players gs)
                startPos = (bigBlindIndex gs + 1) `mod` pCount
                sbPlayer = players gs !! startPos
                activePs = activePlayers gs
                activeIdx = findIndexCustom (playerIndex sbPlayer) (map playerIndex activePs)
            in case activeIdx of
                Just idx -> idx
                Nothing  -> 0
        _ ->
            let pCount = length (players gs)
                startPos = (dealerPos gs + 1) `mod` pCount
                dealerPlayer = players gs !! startPos
                activePs = activePlayers gs
                activeIdx = findIndexCustom (playerIndex dealerPlayer) (map playerIndex activePs)
            in case activeIdx of
                Just idx -> idx
                Nothing  -> 0

-- | Handle a betting round
bettingRound :: GameState -> IO GameState
bettingRound gs = do
    let startingIdx = getStartingPlayerIndex gs
    bettingLoop gs startingIdx 0
  where
    bettingLoop :: GameState -> Int -> Int -> IO GameState
    bettingLoop state currentPlayerRound raisesThisRound = do
        if length (activePlayers state) <= 1
            then return state
            else do
                let allMatched = all (\p -> (isPlaying p && not (hasFolded p) && (bets state !! playerIndex p) == highestBet state)) (activePlayers state)
                if allMatched && raisesThisRound >= length (activePlayers state)
                    then do
                        let committedState = commitBetsToPot state
                        return committedState
                    else do
                        let activeCount = length (activePlayers state)
                            playerIdx = currentPlayerRound `mod` activeCount
                            currentPlayer = activePlayers state !! playerIdx
                        if not (isPlaying currentPlayer) || hasFolded currentPlayer
                            then bettingLoop state (currentPlayerRound + 1) raisesThisRound
                            else do
                                newState <- performStrategyAction state playerIdx
                                let currentAction = action (players newState !! playerIndex currentPlayer)
                                let newRaises = if currentAction == Raise then raisesThisRound + 1 else raisesThisRound
                                bettingLoop newState (currentPlayerRound + 1) newRaises

    -- Commit all current bets to the pot
    commitBetsToPot :: GameState -> GameState
    commitBetsToPot gs =
        let totalBets = sum (bets gs)
            updatedPot = pot gs + totalBets
            resetBets = replicate (length (bets gs)) 0
        in gs { pot = updatedPot, bets = resetBets }

-- | Mark a player's action
markPlayerAction :: GameState -> Int -> Action -> [Player]
markPlayerAction gs pIdx act =
    let player = players gs !! pIdx
        updatedPlayer = player { action = act }
    in replacePlayerAt (players gs) pIdx updatedPlayer

-- | Random strategy action
randomAction :: GameState -> Int -> IO GameState
randomAction gs activePIdx = do
    let player = activePlayers gs !! activePIdx
        globalIdx = playerIndex player
        currentBet = highestBet gs
        playerCurrentBet = (bets gs) !! globalIdx
    if currentBet == playerCurrentBet
        then if currentBet == 0
             then do
                 decision <- randomInRange 0 2
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerChecks gs globalIdx)
                     2 -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

             else do
                 decision <- randomInRange 0 1
                 if decision == 0
                     then return (playerFolds gs activePIdx)
                     else do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

        else if currentBet == 0
             then do
                 decision <- randomInRange 0 2
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerChecks gs globalIdx)
                     2 -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

             else do
                 decision <- randomInRange 0 2
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerCalls gs globalIdx)
                     2 -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

-- | passive behaviour (pussy)
passiveAction :: GameState -> Int -> IO GameState
passiveAction gs activePIdx = do
    let player = activePlayers gs !! activePIdx
        globalIdx = playerIndex player
        currentBet = highestBet gs
        playerCurrentBet = (bets gs) !! globalIdx
    if currentBet == playerCurrentBet
        then if currentBet == 0
             then do
                 decision <- randomInRange 0 1
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerChecks gs globalIdx)
             else do
                 decision <- randomInRange 0 1
                 if decision == 0
                     then return (playerFolds gs activePIdx)
                     else return (playerCalls gs globalIdx)
        else if currentBet == 0
             then do
                 decision <- randomInRange 0 1
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerChecks gs globalIdx)
             else do
                 decision <- randomInRange 0 1
                 if decision == 0
                     then return (playerFolds gs activePIdx)
                     else return (playerCalls gs globalIdx)

-- | aggressive action behavior (all ins raises and more often)
aggressiveAction :: GameState -> Int -> IO GameState
aggressiveAction gs activePIdx = do
    let player = activePlayers gs !! activePIdx
        globalIdx = playerIndex player
        currentBet = highestBet gs
        playerCurrentBet = (bets gs) !! globalIdx
    if currentBet == playerCurrentBet
        then if currentBet == 0
             then do
                 decision <- randomInRange 0 4
                 case decision of
                     0 -> return (playerChecks gs globalIdx)
                     1 -> return (playerChecks gs globalIdx)
                     _ -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

             else do
                 decision <- randomInRange 0 4
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerCalls gs globalIdx)
                     2 -> return (playerCalls gs globalIdx)
                     _ -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

        else if currentBet == 0
             then do
                 decision <- randomInRange 0 4
                 case decision of
                     0 -> return (playerChecks gs globalIdx)
                     1 -> return (playerChecks gs globalIdx)
                     _ -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)

             else do
                 decision <- randomInRange 0 4
                 case decision of
                     0 -> return (playerFolds gs activePIdx)
                     1 -> return (playerCalls gs globalIdx)
                     2 -> return (playerCalls gs globalIdx)
                     _ -> do
                         raiseAmount <- determineRaiseAmount gs globalIdx
                         return (playerRaises gs globalIdx raiseAmount)


-- | behaviour for smart strategy
smartAction :: GameState -> Int -> IO GameState
smartAction gs activePIdx = do
    let player = activePlayers gs !! activePIdx
        globalIdx = playerIndex player
        currentPhase = gamePhase gs
        currentBet = highestBet gs
        playerCurrentBet = (bets gs) !! globalIdx
        isCurrentBetMatched = (currentBet == playerCurrentBet)
    handStrength <- case currentPhase of
        PreFlop -> return $ determineHandWinProbability (hand player)
        Flop -> do
            let comm = take 3 (communityCards gs)
                (_, handRank) = evaluateBestHand5 (hand player ++ comm)
            return $ calculateHandValue Flop handRank
        Turn -> do
            let comm = take 4 (communityCards gs)
                (_, handRank) = evaluateBestHand6 (hand player ++ comm)
            return $ calculateHandValue Turn handRank
        River -> do
            let comm = communityCards gs
                (_, handRank) = evaluatePlayerHand (hand player) comm
            return $ calculateHandValue River handRank
    decisionProbability <- randomInRange 1 100
    if isCurrentBetMatched
        then
            if currentPhase == PreFlop
                then if decisionProbability <= handStrength
                        then do
                            raiseAmount <- determineRaiseAmount gs globalIdx
                            return (playerRaises gs globalIdx raiseAmount)

                        else return (playerChecks gs globalIdx)
                else if decisionProbability <= handStrength
                        then do
                            raiseAmount <- determineRaiseAmount gs globalIdx
                            return (playerRaises gs globalIdx raiseAmount)

                        else return (playerChecks gs globalIdx)
        else
            if currentPhase == PreFlop
                then if decisionProbability <= handStrength
                        then do
                            raiseAmount <- determineRaiseAmount gs globalIdx
                            return (playerRaises gs globalIdx raiseAmount)

                        else if decisionProbability <= handStrength + 20
                            then return (playerCalls gs globalIdx)
                            else return (playerFolds gs activePIdx)
                else if decisionProbability <= handStrength
                        then do
                            raiseAmount <- determineRaiseAmount gs globalIdx
                            return (playerRaises gs globalIdx raiseAmount)
                        else if decisionProbability <= handStrength + 20
                            then return (playerCalls gs globalIdx)
                            else return (playerFolds gs activePIdx)

-- | Determine the probability of a starting hand winning
determineHandWinProbability :: [Card] -> Int
determineHandWinProbability hand
    | length hand /= 2 = error "A starting hand must consist of exactly two cards."
    | isPair sortedHand = case rank (head sortedHand) of
        Ace   -> 74; King  -> 68; Queen -> 63; Jack  -> 59; Ten  -> 55
        Nine  -> 51; Eight -> 48; Seven -> 44; Six   -> 42; Five -> 39
        Four  -> 36; Three -> 34; Two   -> 32
    | isSuitedHand sortedHand = case (rank (head sortedHand), rank (sortedHand !! 1)) of -- this determines the win probability of each starting hand in a holdem game of 4 players (+10% for optimism)
        (Ace, King) -> 52; (Ace, Queen) -> 51; (Ace, Jack) -> 49; (Ace, Ten) -> 48
        (Ace, Nine) -> 46; (Ace, Eight) -> 45; (Ace, Seven) -> 44; (Ace, Six) -> 43
        (Ace, Five) -> 43; (Ace, Four) -> 43; (Ace, Three) -> 42; (Ace, Two) -> 41
        (King, Queen) -> 49; (King, Jack) -> 48; (King, Ten) -> 47; (King, Nine) -> 44
        (King, Eight) -> 42; (King, Seven) -> 41; (King, Six) -> 40; (King, Five) -> 40
        (King, Four) -> 39; (King, Three) -> 38; (King, Two) -> 38
        (Queen, Jack) -> 47; (Queen, Ten) -> 45; (Queen, Nine) -> 43; (Queen, Eight) -> 41
        (Queen, Seven) -> 39; (Queen, Six) -> 38; (Queen, Five) -> 38; (Queen, Four) -> 37
        (Queen, Three) -> 36; (Queen, Two) -> 36
        (Jack, Ten) -> 45; (Jack, Nine) -> 42; (Jack, Eight) -> 40; (Jack, Seven) -> 38
        (Jack, Six) -> 37; (Jack, Five) -> 36; (Jack, Four) -> 37; (Jack, Three) -> 35
        (Ten, Nine) -> 42; (Ten, Eight) -> 40; (Ten, Seven) -> 38; (Ten, Six) -> 36
        (Ten, Five) -> 35; (Ten, Four) -> 34; (Ten, Three) -> 33; (Ten, Two) -> 33
        (Nine, Eight) -> 40; (Nine, Seven) -> 38; (Nine, Six) -> 36; (Nine, Five) -> 35
        (Nine, Four) -> 33; (Nine, Three) -> 32; (Nine, Two) -> 31
        (Eight, Seven) -> 38; (Eight, Six) -> 36; (Eight, Five) -> 35; (Eight, Four) -> 33
        (Eight, Three) -> 31; (Eight, Two) -> 30
        (Seven, Six) -> 33; (Seven, Five) -> 31; (Seven, Four) -> 29; (Seven, Three) -> 27
        (Seven, Two) -> 27
        (Six, Five) -> 35; (Six, Four) -> 33; (Six, Three) -> 32; (Six, Two) -> 30
        (Five, Four) -> 30; (Five, Three) -> 29; (Five, Two) -> 27
        (Four, Three) -> 27; (Four, Two) -> 26
        (Three, Two) -> 25
        _ -> 25
    | otherwise = case (rank (head sortedHand), rank (sortedHand !! 1)) of
        (Ace, King) -> 49; (Ace, Queen) -> 48; (Ace, Jack) -> 46; (Ace, Ten) -> 48
        (Ace, Nine) -> 42; (Ace, Eight) -> 41; (Ace, Seven) -> 40; (Ace, Six) -> 39
        (Ace, Five) -> 40; (Ace, Four) -> 39; (Ace, Three) -> 38; (Ace, Two) -> 38
        (King, Queen) -> 46; (King, Jack) -> 45; (King, Ten) -> 43; (King, Nine) -> 41
        (King, Eight) -> 38; (King, Seven) -> 38; (King, Six) -> 40; (King, Five) -> 40
        (King, Four) -> 39; (King, Three) -> 38; (King, Two) -> 38
        (Queen, Jack) -> 43; (Queen, Ten) -> 42; (Queen, Nine) -> 40; (Queen, Eight) -> 41
        (Queen, Seven) -> 39; (Queen, Six) -> 38; (Queen, Five) -> 38; (Queen, Four) -> 37
        (Queen, Three) -> 36; (Queen, Two) -> 36
        (Jack, Ten) -> 45; (Jack, Nine) -> 42; (Jack, Eight) -> 40; (Jack, Seven) -> 38
        (Jack, Six) -> 37; (Jack, Five) -> 36; (Jack, Four) -> 37; (Jack, Three) -> 35
        (Ten, Nine) -> 42; (Ten, Eight) -> 40; (Ten, Seven) -> 38; (Ten, Six) -> 36
        (Ten, Five) -> 35; (Ten, Four) -> 34; (Ten, Three) -> 33; (Ten, Two) -> 33
        (Nine, Eight) -> 40; (Nine, Seven) -> 38; (Nine, Six) -> 36; (Nine, Five) -> 35
        (Nine, Four) -> 33; (Nine, Three) -> 32; (Nine, Two) -> 31
        (Eight, Seven) -> 38; (Eight, Six) -> 36; (Eight, Five) -> 35; (Eight, Four) -> 33
        (Eight, Three) -> 31; (Eight, Two) -> 30
        (Seven, Six) -> 33; (Seven, Five) -> 31; (Seven, Four) -> 29; (Seven, Three) -> 27
        (Seven, Two) -> 27
        (Six, Five) -> 35; (Six, Four) -> 33; (Six, Three) -> 32; (Six, Two) -> 30
        (Five, Four) -> 30; (Five, Three) -> 29; (Five, Two) -> 27
        (Four, Three) -> 27; (Four, Two) -> 26
        (Three, Two) -> 25
        _ -> 25
  where
    isPairHand :: [Card] -> Bool
    isPairHand [Card r1 _, Card r2 _] = r1 == r2
    isPairHand _ = False

    isSuitedHand :: [Card] -> Bool
    isSuitedHand [Card _ s1, Card _ s2] = s1 == s2
    isSuitedHand _ = False

    sortedHand :: [Card]
    sortedHand =
        if getRankValue (rank (head hand)) >= getRankValue (rank (hand !! 1))
            then hand
            else [hand !! 1, head hand]

-- | determine the amount to raise based on current game state
determineRaiseAmount :: GameState -> Int -> IO Int
determineRaiseAmount gs pIndex = do
    let highestB = highestBet gs
        player = players gs !! pIndex
        availableChips = chips player
    if availableChips <= highestB
        then return highestB
        else return (highestB + 20)

-- | assign a player action based on their strategy
performActionForStrategy :: GameState -> Int -> IO GameState
performActionForStrategy gs activePlayerIdx = do
    let player = activePlayers gs !! activePlayerIdx
    case strategy player of
        RandomStrategy     -> randomAction gs activePlayerIdx
        AggressiveStrategy -> aggressiveAction gs activePlayerIdx
        PassiveStrategy    -> passiveAction gs activePlayerIdx
        SmartStrategy      -> smartAction gs activePlayerIdx




-- | replace a list  of players with updated players
replacePlayers :: [Player] -> [Player] -> [Player]
replacePlayers = foldl replace
  where
    replace :: [Player] -> Player -> [Player]
    replace pls upd = replacePlayerAt pls (playerIndex upd) upd

-- | resets the folded status and action of all players
resetFoldedStatus :: GameState -> GameState
resetFoldedStatus gs =
    let updatedPlayers = map resetPlayer (players gs)
        resetPlayer p = p { hasFolded = False, action = NullAction }
    in gs { players = updatedPlayers }

-- | Commit all current bets to the pot
commitBetsToPot :: GameState -> GameState
commitBetsToPot gs =
    let totalBets = sum (bets gs)
        updatedPot = pot gs + totalBets
        resetBets = replicate (length (bets gs)) 0
    in gs { pot = updatedPot, bets = resetBets }

-- | Handle a player folding
playerHasFolded :: GameState -> Int -> GameState
playerHasFolded = playerFolds

-- | performs a loop of a hand of poker
gameLoop :: Int -> Int -> GameState -> IO GameState
gameLoop currentRound maxRounds gs
    | currentRound > maxRounds = return gs
    | length (activePlayers gs) <= 1 = return gs
    | otherwise = do
        let dealtState = dealCards gs
        let afterBlinds = postBlinds dealtState
        afterBetting <- bettingRound afterBlinds
        let afterCommit = commitBetsToPot afterBetting
        let afterWin = determineRoundWinner afterCommit
        let afterReset = resetFoldedStatus afterWin

        -- rotate dealer and blinds
        let totalPlayers = length (players afterWin)
            newDealerPos = (dealerPos afterWin + 1) `mod` totalPlayers
            newSmallBlindIdx = (newDealerPos + 1) `mod` totalPlayers
            newBigBlindIdx = (newDealerPos + 2) `mod` totalPlayers
            updatedPlayers = map (\p -> if playerIndex p == newDealerPos then p { isDealer = True } else p { isDealer = False }) (players afterWin)
            resetBets = replicate (length updatedPlayers) 0
            resetContributions = replicate (length updatedPlayers) 0

        -- Create new game state for the next round
        let newGameState = afterReset {
                players = updatedPlayers,
                activePlayers = getActivePlayers updatedPlayers,
                dealerPos = newDealerPos,
                smallBlindIndex = newSmallBlindIdx,
                bigBlindIndex = newBigBlindIdx,
                deck = deck afterWin,
                communityCards = [],
                pot = 0,
                bets = resetBets,
                contributions = resetContributions,
                gamePhase = PreFlop,
                revealedCommunityCards = [],
                roundWinner = Nothing,
                roundWinnerHand = Nothing
            }

        gameLoop (currentRound + 1) maxRounds newGameState

-- | run multiple games and track win counts
runGames :: Int -> GameState -> [Int] -> IO [Int]
runGames 0 _ counts = return counts
runGames n gs counts = do
    let resetPlayers = map resetPlayer (players gs)
        resetPlayer p = p { chips = defaultChips, isPlaying = True, hasFolded = False, action = NullAction, isDealer = False }

    let dealerIdx = dealerPos gs
    let updatedResetPlayers = map (\p -> if playerIndex p == dealerIdx then p { isDealer = True } else p { isDealer = False }) resetPlayers

    shuffledDeck <- shuffleDeck generateDeck

    let resetGameState = gs {
            players = updatedResetPlayers,
            activePlayers = getActivePlayers updatedResetPlayers,
            deck = shuffledDeck,
            communityCards = [],
            pot = 0,
            bets = replicate (length updatedResetPlayers) 0,
            contributions = replicate (length updatedResetPlayers) 0,
            gamePhase = PreFlop,
            revealedCommunityCards = [],
            roundWinner = Nothing,
            roundWinnerHand = Nothing
        }

    -- runs a gameloop 100 times (or untill 1 player has all chips)
    finalGS <- gameLoop 1 100 resetGameState

    let winner = case roundWinner finalGS of
                    Just p  -> playerIndex p
                    Nothing -> playerIndex $ head $ sortByCustom (\x y -> compare (chips y) (chips x)) (players finalGS)

    -- update win counts
    let updatedCounts = setElementAt winner ((counts !! winner) + 1) counts

    runGames (n - 1) finalGS updatedCounts

-- | Main function to run the simulation
main :: IO ()
main = do
    -- Create players with different strategies
    let player1 = createPlayer "Passive" True 0 SmartStrategy
    let player2 = createPlayer "Aggressive" False 1 AggressiveStrategy
    let player3 = createPlayer "Random" False 2 SmartStrategy
    let player4 = createPlayer "Smart" False 3 SmartStrategy

    let initialPlayers = [player1, player2, player3, player4]

    -- shuffle deck 
    shuffledDeck <- shuffleDeck generateDeck

    -- initialise  game state
    let initialGameState = GameState {
            players = initialPlayers,
            activePlayers = initialPlayers,
            deck = shuffledDeck,
            communityCards = [],
            pot = 0,
            bets = replicate (length initialPlayers) 0,
            contributions = replicate (length initialPlayers) 0,
            dealerPos = 0,
            gamePhase = PreFlop,
            revealedCommunityCards = [],
            roundWinner = Nothing,
            roundWinnerHand = Nothing,
            smallBlindIndex = 1,
            bigBlindIndex = 2
        }

    let initialWinCounts = replicate (length initialPlayers) 0

    -- CHANGE THIS TO CHANGE NUMBER OF SIMULATIONS
    let numberOfGames = 1000

    finalWinCounts <- runGames numberOfGames initialGameState initialWinCounts

    putStrLn "\n=== Simulation Results ==="
    mapM_ (\(p, cnt) -> putStrLn $ name p ++ ": " ++ show cnt) (zip initialPlayers finalWinCounts)
    putStrLn "=========================="
