{-# LANGUAGE ScopedTypeVariables #-}

import System.Random
import Data.List (sortBy)
import Data.Ord (comparing)

-- Game Parameters
defaultChips :: Int
defaultChips = 1000

smallBlind :: Int
smallBlind = 50

bigBlind :: Int
bigBlind = 100

-- Manual Implementations of List Functions

-- Manual implementation of intercalate
myIntercalate :: String -> [String] -> String
myIntercalate _ []     = ""
myIntercalate _ [x]    = x
myIntercalate sep (x:xs) = x ++ sep ++ myIntercalate sep xs

-- Manual implementation of insertion sort for [Card]
mySort :: [Card] -> [Card]
mySort []     = []
mySort (x:xs) = insert x (mySort xs)
  where
    insert :: Card -> [Card] -> [Card]
    insert card [] = [card]
    insert card (y:ys)
        | getCardValue card <= getCardValue y = card : y : ys
        | otherwise = y : insert card ys

-- Manual implementation of insertion sort for [Int]
mySortInt :: [Int] -> [Int]
mySortInt []     = []
mySortInt (x:xs) = insertInt x (mySortInt xs)
  where
    insertInt :: Int -> [Int] -> [Int]
    insertInt n [] = [n]
    insertInt n (y:ys)
        | n <= y    = n : y : ys
        | otherwise = y : insertInt n ys

-- Manual implementation of nub
myNub :: Eq a => [a] -> [a]
myNub []     = []
myNub (x:xs) = x : myNub (remove x xs)
  where
    remove :: Eq a => a -> [a] -> [a]
    remove _ [] = []
    remove a (y:ys)
        | a == y    = remove a ys
        | otherwise = y : remove a ys

-- Manual implementation of findIndex
findIndex' :: Eq a => a -> [a] -> Maybe Int
findIndex' _ [] = Nothing
findIndex' x (y:ys)
    | x == y    = Just 0
    | otherwise = case findIndex' x ys of
                    Just n  -> Just (n + 1)
                    Nothing -> Nothing

-- Helper function to set an element at a specific index
setElement :: Int -> a -> [a] -> [a]
setElement _ _ [] = []
setElement 0 newVal (_:xs) = newVal : xs
setElement n newVal (x:xs) = x : setElement (n-1) newVal xs

-- Helper function to replace a player in the players list at a specific index
replacePlayer :: [Player] -> Int -> Player -> [Player]
replacePlayer ps i newP = take i ps ++ [newP] ++ drop (i + 1) ps

-- Helper function to remove an element at a specific index
removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

-- randRange Function
randRange :: Int -> Int -> IO Int
randRange a b = randomRIO (a, b)

-- Card Definitions

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq)

instance Show Card where
    show (Card {rank = r, suit = s}) = show r ++ " of " ++ show s

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

getCardValue :: Card -> Int
getCardValue c = getRankValue (rank c)

getDeck :: [Card]
getDeck = [Card r s | s <- [Clubs .. Spades], r <- [Two .. Ace]]

-- Shuffle Deck using recursion and manual removeAt
shuffleDeck :: [Card] -> IO [Card]
shuffleDeck [] = return []
shuffleDeck xs = do
    randIndex <- randRange 0 (length xs - 1)
    let (pickedElement, remainingElements) = removeAtLocal randIndex xs
    rest <- shuffleDeck remainingElements
    return (pickedElement : rest)
  where
    removeAtLocal :: Int -> [a] -> (a, [a])
    removeAtLocal i ys = (ys !! i, take i ys ++ drop (i + 1) ys)

-- Player Definitions

data Action = Check | Raise | Call | Fold | Null
    deriving (Show, Eq)

data Strategy = Random | Aggressive
    deriving (Show, Eq)

data Player = Player {
    name        :: String,
    hand        :: [Card],
    chips       :: Int,
    isPlaying   :: Bool,
    hasFolded   :: Bool,      -- New Field
    action      :: Action,
    isDealer    :: Bool,
    strategy    :: Strategy,
    playerIndex :: Int
} deriving (Eq)

instance Show Player where
    show (Player { name = n, hand = c, chips = ch, isPlaying = p, hasFolded = hf, action = a, isDealer = i, strategy = s, playerIndex = ind}) =
        "Player #" ++ show ind ++ " - " ++ n ++ ":\n" ++
        "  Chips        : " ++ show ch ++ "\n" ++
        "  Status       : " ++ (if p then "Active" else "Out") ++ "\n" ++
        "  Folded       : " ++ (if hf then "Yes" else "No") ++ "\n" ++
        "  Action       : " ++ show a ++ "\n" ++
        "  Cards        : " ++ showCards c ++ "\n" ++
        "  Dealer       : " ++ (if i then "Yes" else "No") ++ "\n" ++
        "  Strategy     : " ++ show s ++ "\n"
      where
        showCards :: [Card] -> String
        showCards []     = "None"
        showCards [card] = show card
        showCards cards  = myIntercalate ", " (map show cards)

customPlayer :: String -> Bool -> Int -> Player
customPlayer playerName dealer pIndex = Player {
    name        = playerName,
    hand        = [],
    chips       = defaultChips,
    isPlaying   = True,
    hasFolded   = False,       -- Initialize as False
    action      = Null,
    isDealer    = dealer,
    strategy    = Random,
    playerIndex = pIndex
}

-- Game Phase Definitions

data GamePhase = PreFlop | Flop | Turn | River
    deriving (Eq, Show)

-- Hand Ranking Definitions

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

-- GameState Definition

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
    roundWinnerHand        :: Maybe HandRanking, -- New Field Added
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
        if null (activePlayers gs)
            then "  None\n"
            else concatMap show (activePlayers gs) ++
        "\nCommunity Cards:\n  " ++ showCards (communityCards gs) ++ "\n" ++
        "\nBets:\n  " ++ show (bets gs) ++ "\n" ++
        "Contributions:\n  " ++ show (contributions gs) ++ "\n" ++
        "\nRound Winner:\n  " ++ showWinnerWithHand (roundWinner gs, roundWinnerHand gs) ++ "\n" ++
        "=================="
      where
        showCards :: [Card] -> String
        showCards []     = "None"
        showCards [card] = show card
        showCards cards  = myIntercalate ", " (map show cards)
        
        showWinnerWithHand :: (Maybe Player, Maybe HandRanking) -> String
        showWinnerWithHand (Nothing, _) = "None"
        showWinnerWithHand (Just winner, Just handType) = name winner ++ " (Player #" ++ show (playerIndex winner) ++ ") - " ++ show handType
        showWinnerWithHand (Just winner, Nothing) = name winner ++ " (Player #" ++ show (playerIndex winner) ++ ")"

-- Utility Functions for GameState

getActivePlayers :: [Player] -> [Player]
getActivePlayers = myNub . filter (\p -> isPlaying p && not (hasFolded p))

highestBet :: GameState -> Int
highestBet gs = if null (bets gs) then 0 else maximum (bets gs)

setGamePhase :: GameState -> GamePhase -> GameState
setGamePhase gameState newGamePhase = gameState { gamePhase = newGamePhase }

-- Eliminate Players with 0 or fewer chips
eliminatePlayers :: GameState -> GameState
eliminatePlayers gs =
    let updatedPlayers = map eliminateIfZero (players gs)
        eliminateIfZero p
            | chips p <= 0 = p { isPlaying = False }
            | otherwise    = p
        updatedActivePlayers = getActivePlayers updatedPlayers
    in gs { players = updatedPlayers, activePlayers = updatedActivePlayers }
    where
        eliminateIfZero :: Player -> Player
        eliminateIfZero p
            | chips p <= 0 = p { isPlaying = False }
            | otherwise    = p

-- Posting Blinds
postBlinds :: GameState -> GameState
postBlinds gs =
    let pCount = length (players gs)
        sbPlayer = players gs !! (smallBlindIndex gs)
        bbPlayer = players gs !! (bigBlindIndex gs)
        sbAmount = min smallBlind (chips sbPlayer)
        bbAmount = min bigBlind (chips bbPlayer)
        sbPlayer' = sbPlayer { chips = chips sbPlayer - sbAmount }
        bbPlayer' = bbPlayer { chips = chips bbPlayer - bbAmount }
        updatedPlayers = replacePlayer (replacePlayer (players gs) (smallBlindIndex gs) sbPlayer') (bigBlindIndex gs) bbPlayer'
        updatedBets = bets gs
        updatedBets' = setElement (smallBlindIndex gs) sbAmount updatedBets
        updatedBets'' = setElement (bigBlindIndex gs) bbAmount updatedBets'
        updatedContributions = contributions gs
        updatedContributions' = setElement (smallBlindIndex gs) sbAmount (setElement (bigBlindIndex gs) bbAmount updatedContributions)
        updatedActivePlayers = getActivePlayers updatedPlayers
    in gs { players = updatedPlayers,
            activePlayers = updatedActivePlayers,
            bets = updatedBets'',
            contributions = updatedContributions' }

-- Dealing Cards
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

dealCards :: GameState -> GameState
dealCards gameState =
    let
        activePs = activePlayers gameState
        numPlayers = length activePs
        cardsToDealToPlayers = numPlayers * 2
        (playerCards, deckAfterPlayers) = splitAt cardsToDealToPlayers (deck gameState)
        (community, remainingDeck) = splitAt 5 deckAfterPlayers
        cardPairs = chunksOf 2 playerCards
        updatedPlayers = zipWith (\player crds -> player { hand = crds }) activePs cardPairs
        updatedAllPlayers = replacePlayers (players gameState) updatedPlayers
        updatedActivePlayers = getActivePlayers updatedAllPlayers
    in
        gameState {
            players = updatedAllPlayers,
            activePlayers = updatedActivePlayers,
            deck = remainingDeck,
            communityCards = community
        }
    where
        replacePlayers :: [Player] -> [Player] -> [Player]
        replacePlayers original updated = foldl replace original updated
            where
                replace :: [Player] -> Player -> [Player]
                replace pls upd = replacePlayer pls (playerIndex upd) upd

-- Determining Next Phases and Revealing Cards
getNextGamePhase :: GamePhase -> GamePhase
getNextGamePhase currentPhase
    | currentPhase == PreFlop = Flop
    | currentPhase == Flop    = Turn
    | currentPhase == Turn    = River
    | currentPhase == River   = PreFlop

getNextRevealedCards :: [Card] -> GameState -> [Card]
getNextRevealedCards currentRevealed gameState
    | length currentRevealed == 0 = take 3 (communityCards gameState)
    | length currentRevealed == 3 = take 4 (communityCards gameState)
    | length currentRevealed == 4 = take 5 (communityCards gameState)
    | otherwise = currentRevealed

advancePhase :: GameState -> GameState
advancePhase gs =
    let newPhase = getNextGamePhase (gamePhase gs)
        newRevealed = getNextRevealedCards (revealedCommunityCards gs) gs
    in gs { gamePhase = newPhase, revealedCommunityCards = newRevealed }

-- Player Actions

playerHasFolded :: GameState -> Int -> GameState
playerHasFolded gs foldedActiveIndex =
    let ap = activePlayers gs
        foldedPlayer = ap !! foldedActiveIndex
        globalIndex = playerIndex foldedPlayer
        p = (players gs) !! globalIndex
        p' = p { hasFolded = True, action = Fold }
        updatedPlayers = replacePlayer (players gs) globalIndex p'
        gsAfterFold = gs { players = updatedPlayers }
    in eliminatePlayers gsAfterFold

playerCalls :: GameState -> Int -> GameState
playerCalls gs pIndex =
    let
        hb = highestBet gs
        currentCycleBet = (bets gs) !! pIndex
        to_call = hb - currentCycleBet
        p = (players gs) !! pIndex
        actualCall = min to_call (chips p)
        newChips = chips p - actualCall
        p' = p { chips = newChips, action = Call }
        updatedPlayers = replacePlayer (players gs) pIndex p'
        newBets = setElement pIndex hb (bets gs)
        newContributions = contributions gs
        newContributions' = setElement pIndex (contributions gs !! pIndex + actualCall) newContributions
        updatedActivePlayers = getActivePlayers updatedPlayers
    in eliminatePlayers gs { players = updatedPlayers, bets = newBets, contributions = newContributions', activePlayers = updatedActivePlayers }

playerChecks :: GameState -> Int -> GameState
playerChecks gs pIndex =
    let hb = highestBet gs
    in if hb == 0
       then eliminatePlayers gs { players = markAction gs pIndex Check }
       else gs 

playerRaises :: GameState -> Int -> Int -> GameState
playerRaises gs pIndex raiseTo =
    let
        currentBet = (bets gs) !! pIndex
        to_raise = raiseTo - currentBet
        p = (players gs) !! pIndex
        actualRaise = min to_raise (chips p)
        newChips = chips p - actualRaise
        p' = p { chips = newChips, action = Raise }
        updatedPlayers = replacePlayer (players gs) pIndex p'
        newBets = setElement pIndex raiseTo (bets gs)
        newContributions = contributions gs
        newContributions' = setElement pIndex (contributions gs !! pIndex + actualRaise) newContributions
        updatedActivePlayers = getActivePlayers updatedPlayers
    in eliminatePlayers gs { players = updatedPlayers, bets = newBets, contributions = newContributions', activePlayers = updatedActivePlayers }


isPair :: [Card] -> Bool
isPair cards =
    let vals = countValues cards
        uniqueVals = myNub vals
        pairs = length (filter (\u -> length (filter (==u) vals) == 2) uniqueVals)
    in pairs >= 1

isTwoPair :: [Card] -> Bool
isTwoPair cards =
    let vals = countValues cards
        uniqueVals = myNub vals
        pairs = length (filter (\u -> length (filter (==u) vals) == 2) uniqueVals)
    in pairs >= 2

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards =
    let groups = valueGroups cards
    in (length (filter (==3) groups) >= 1) && not (isFullHouse cards)

isStraight :: [Card] -> Bool
isStraight cards =
    let values = mySortInt (map getCardValue cards)
        distinctVals = myNub values
    in if length distinctVals < 5 then False
       else checkStraight distinctVals

checkStraight :: [Int] -> Bool
checkStraight vals
    | length vals < 5 = False
    | otherwise =
        let windows5 = takeSliding 5 vals
        in any isSequential windows5

takeSliding :: Int -> [a] -> [[a]]
takeSliding _ [] = []
takeSliding n xs
    | length xs < n = []
    | otherwise     = take n xs : takeSliding n (tail xs)

isSequential :: [Int] -> Bool
isSequential xs = all (\(a, b) -> b == a + 1) (zip xs (tail xs))

isFlush :: [Card] -> Bool
isFlush cards
    | length cards < 5 = False
    | otherwise = length (filter ((== suit (head cards)) . suit) cards) >= 5

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isFlush cards && isStraight cards

isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards =
    isStraightFlush cards && (mySortInt (map getCardValue cards) == [10,11,12,13,14])

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = 4 `elem` valueGroups cards

isFullHouse :: [Card] -> Bool
isFullHouse cards =
    let groups = valueGroups cards
    in mySortInt groups == [2,3]

countValues :: [Card] -> [Int]
countValues cards = map getCardValue cards

valueGroups :: [Card] -> [Int]
valueGroups cards =
    let vals = countValues cards
        counts = map (\v -> length (filter (==v) vals)) vals
    in mySortInt counts

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

bestHandCombo :: [([Card], HandRanking)] -> ([Card], HandRanking)
bestHandCombo combos =
    let sorted = sortBy (comparing snd) combos
    in last sorted 

bestHandPlayer :: [(Player, ([Card], HandRanking))] -> (Player, ([Card], HandRanking))
bestHandPlayer playerHands =
    let sorted = sortBy (comparing (snd . snd)) playerHands
    in last sorted 

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

evaluateHand :: [Card] -> [Card] -> ([Card], HandRanking)
evaluateHand playerHand commCards =
    let allCards = playerHand ++ commCards
        combos = choose 5 allCards
        rankedCombos = [(combo, handRanking combo) | combo <- combos]
    in if null rankedCombos
       then ([], HighCard) 
       else bestHandCombo rankedCombos

determineWinner :: GameState -> GameState
determineWinner gs =
    let comm = communityCards gs
        active = activePlayers gs
        playerResults = [(p, evaluateHand (hand p) comm) | p <- active]
    in if length active == 1
       then
           let winnerPlayer = head active
               totalPot = sum (contributions gs)
               updatedWinner = winnerPlayer { chips = chips winnerPlayer + totalPot }
               updatedPlayers = replacePlayer (players gs) (playerIndex winnerPlayer) updatedWinner
               gsAfterWinner = gs { players = updatedPlayers, pot = 0, roundWinner = Just updatedWinner, roundWinnerHand = Nothing }
               finalGS = eliminatePlayers gsAfterWinner
           in finalGS
       else if null playerResults
           then gs { roundWinner = Nothing } 
           else
               let (winnerPlayer, bestResult) = bestHandPlayer playerResults
                   (_, bestRank) = bestResult
                   totalPot = sum (contributions gs)
                   updatedWinner = winnerPlayer { chips = chips winnerPlayer + totalPot }
                   updatedPlayers = replacePlayer (players gs) (playerIndex winnerPlayer) updatedWinner
                   gsAfterWinner = gs { players = updatedPlayers, pot = 0, roundWinner = Just updatedWinner, roundWinnerHand = Just bestRank }
                   finalGS = eliminatePlayers gsAfterWinner
               in finalGS

setChips :: Player -> Int -> Player
setChips pl ch = pl { chips = ch }


betsForActive :: GameState -> [Int]
betsForActive gs =
    let activeIx = map playerIndex (activePlayers gs)
    in map (\ix -> (bets gs) !! ix) activeIx

startingPlayerIndex :: GameState -> Int
startingPlayerIndex gs =
    case gamePhase gs of
        PreFlop ->
            let pCount = length (players gs)
                startPos = (bigBlindIndex gs + 1) `mod` pCount
                sbPlayer = players gs !! startPos
                ap = activePlayers gs
                apIndex = findIndex' (playerIndex sbPlayer) (map playerIndex ap)
            in case apIndex of
                Just idx -> idx
                Nothing  -> 0
        _ ->
            let pCount = length (players gs)
                startPos = (dealerPos gs + 1) `mod` pCount
                dealerPlayer = players gs !! startPos
                ap = activePlayers gs
                apIndex = findIndex' (playerIndex dealerPlayer) (map playerIndex ap)
            in case apIndex of
                Just idx -> idx
                Nothing  -> 0 

randomAction :: GameState -> Int -> IO GameState
randomAction gs activePIndex = do
    let p = (activePlayers gs) !! activePIndex
    let globalIndex = playerIndex p
    let hb = highestBet gs

    if hb == (bets gs !! globalIndex)
        then do
            if hb == 0
                then do
                    n <- randRange 0 2
                    case n of
                        0 -> return (playerHasFolded gs activePIndex)
                        1 -> return (playerChecks gs globalIndex)
                        2 -> do
                            raiseAmt <- pickRaiseAmount gs globalIndex
                            let gsRaised = playerRaises gs globalIndex raiseAmt
                            return gsRaised
                else do
                    n <- randRange 0 1
                    if n == 0
                        then return (playerHasFolded gs activePIndex)
                        else do
                            raiseAmt <- pickRaiseAmount gs globalIndex
                            let gsRaised = playerRaises gs globalIndex raiseAmt
                            return gsRaised
        else if hb == 0
            then do
                n <- randRange 0 2
                case n of
                    0 -> return (playerHasFolded gs activePIndex)
                    1 -> return (playerChecks gs globalIndex)
                    2 -> do
                        raiseAmt <- pickRaiseAmount gs globalIndex
                        let gsRaised = playerRaises gs globalIndex raiseAmt
                        return gsRaised
            else do
                n <- randRange 0 2
                case n of
                    0 -> return (playerHasFolded gs activePIndex)
                    1 -> do
                        let gsCalled = playerCalls gs globalIndex
                        return gsCalled
                    2 -> do
                        raiseAmt <- pickRaiseAmount gs globalIndex
                        let gsRaised = playerRaises gs globalIndex raiseAmt
                        return gsRaised

pickRaiseAmount :: GameState -> Int -> IO Int
pickRaiseAmount gs pIndex = do
    let hb = highestBet gs
    let p = (players gs) !! pIndex
    let maxRaise = chips p
    if maxRaise <= hb
        then return hb
        else randRange (hb + 10) (hb + min 100 (maxRaise - hb)) 

markAction :: GameState -> Int -> Action -> [Player]
markAction gs pIndex act =
    let p = (players gs) !! pIndex
        p' = p { action = act }
    in replacePlayer (players gs) pIndex p'

bettingRound :: GameState -> IO GameState
bettingRound gs = do
    let startIndex = startingPlayerIndex gs
    bettingLoop gs startIndex False 0
  where
    bettingLoop :: GameState -> Int -> Bool -> Int -> IO GameState
    bettingLoop state currentPos raiseOccurred countSinceRaise = do
        if length (activePlayers state) <= 1
            then do
                putStrLn "Only one active player remains. Betting round ends."
                return state
            else do
                let hb = highestBet state
                let allMatched = all (\p -> (isPlaying p && not (hasFolded p) && (bets state !! playerIndex p) == hb)) (activePlayers state)
                
                -- Debugging Statements
                putStrLn $ "Current highest bet: " ++ show hb
                putStrLn $ "All matched: " ++ show allMatched
                putStrLn $ "Count since last raise: " ++ show countSinceRaise
                
                if allMatched && countSinceRaise >= length (activePlayers state)
                    then do
                        putStrLn "All players have matched the highest bet. Betting round ends."
                        let finalState = commitBetsToPot state
                        return finalState
                    else do
                        let activeCount = length (activePlayers state)
                        let pIndex = currentPos `mod` activeCount
                        let currentPlayer = (activePlayers state) !! pIndex
                        
                        -- Debugging Statement
                        putStrLn $ "Player acting: " ++ name currentPlayer ++ " (Index: " ++ show pIndex ++ ")"
                        
                        if not (isPlaying currentPlayer) || hasFolded currentPlayer
                            then do
                                putStrLn $ "Player " ++ name currentPlayer ++ " is not active or has folded. Skipping."
                                bettingLoop state (currentPos + 1) raiseOccurred (countSinceRaise + 1)
                            else do
                                stateAfterAction <- if strategy currentPlayer == Random
                                                    then randomAction state pIndex
                                                    else return state -- Only Random strategy implemented
                                
                                let playerAct = actionTaken state stateAfterAction (playerIndex currentPlayer)
                                putStrLn $ "Player " ++ name currentPlayer ++ " performed action: " ++ show playerAct
                                
                                let newRaiseOccurred = (playerAct == Raise) || raiseOccurred
                                let newCountSinceRaise = if playerAct == Raise
                                                         then 0
                                                         else countSinceRaise + 1
                                bettingLoop stateAfterAction (currentPos + 1) newRaiseOccurred newCountSinceRaise

    actionTaken oldState newState pIndex =
        let oldPlayer = (players oldState) !! pIndex
            newPlayer = (players newState) !! pIndex
        in action newPlayer

-- Commit Bets to Pot
commitBetsToPot :: GameState -> GameState
commitBetsToPot gs =
    let currentBets = bets gs
        total = sum currentBets
        newPot = pot gs + total
        clearedBets = replicate (length currentBets) 0
    in gs { pot = newPot, bets = clearedBets }

-- Reset folded status for all players
resetFoldedStatus :: GameState -> GameState
resetFoldedStatus gs =
    let updatedPlayers = map resetFolded (players gs)
        resetFolded p = p { hasFolded = False, action = Null }
    in gs { players = updatedPlayers }

gameLoop :: Int -> Int -> GameState -> IO ()
gameLoop currentRound maxRounds gs
    | currentRound > maxRounds = do
        putStrLn "\n=== Game Over ==="
        putStrLn "Final Player Chip Counts:"
        mapM_ printPlayerChips (players gs)
    | length (activePlayers gs) <= 1 = do
        putStrLn "\n=== Game Over ==="
        putStrLn "Final Player Chip Counts:"
        mapM_ printPlayerChips (players gs)
    | otherwise = do
        putStrLn $ "\n=== Starting Round " ++ show currentRound ++ " ==="

        -- Deal Cards
        let gsAfterDeal = dealCards gs

        -- Post Blinds (PreFlop)
        let gsWithBlinds = postBlinds(gsAfterDeal)

        -- Run Betting Round
        finalGS <- bettingRound gsWithBlinds

        -- Commit Bets to Pot
        let gsAfterBet = commitBetsToPot finalGS

        -- Determine Winner
        let gsWithWinner = determineWinner gsAfterBet

        -- Reset folded status
        let gsReset = resetFoldedStatus gsWithWinner

        -- Print GameState After Determining Winner
        putStrLn "\nFinal GameState After Determining Winner:"
        print gsWithWinner

        -- Announce Winner and Hand Type
        case (roundWinner gsWithWinner, roundWinnerHand gsWithWinner) of
            (Just winner, Just handType) -> putStrLn $ "\nWinner of Round " ++ show currentRound ++ ": " ++ name winner ++ " - " ++ show handType
            (Just winner, Nothing)       -> putStrLn $ "\nWinner of Round " ++ show currentRound ++ ": " ++ name winner
            (Nothing, _)                 -> putStrLn "\nNo Winner Determined for this Round."

        -- Rotate Dealer Position
        let pCount = length (players gsWithWinner)
            newDealerPos = nextDealerPos gsWithWinner
            newSmallBlindIndex = nextBlindPos gsWithWinner newDealerPos
            newBigBlindIndex = nextBlindPos gsWithWinner newSmallBlindIndex

            nextDealerPos :: GameState -> Int
            nextDealerPos state =
                let currentDealer = dealerPos state
                    nextPos = (currentDealer + 1) `mod` pCount
                in findNextActivePlayer state nextPos

            nextBlindPos :: GameState -> Int -> Int
            nextBlindPos state pos =
                let nextPos = (pos + 1) `mod` pCount
                in findNextActivePlayer state nextPos

            findNextActivePlayer :: GameState -> Int -> Int
            findNextActivePlayer state pos =
                if isPlaying (players state !! pos)
                    then pos
                    else findNextActivePlayer state ((pos + 1) `mod` pCount)

            markDealerAndBlinds :: [Player] -> Int -> Int -> Int -> [Player]
            markDealerAndBlinds pls dPos sbPos bbPos =
                let plsWithoutDealer = map (\p -> p { isDealer = False }) pls
                    dealerUpdated = (plsWithoutDealer !! dPos) { isDealer = True }
                    updated = replacePlayer plsWithoutDealer dPos dealerUpdated
                in updated

        let updatedPlayers = markDealerAndBlinds (players gsReset) newDealerPos newSmallBlindIndex newBigBlindIndex


        let resetBets = replicate (length updatedPlayers) 0
            resetContributions = replicate (length updatedPlayers) 0

        let nextRoundGS = gsReset {
                players = updatedPlayers,
                activePlayers = getActivePlayers updatedPlayers,
                deck = deck gsWithWinner,
                communityCards = [],
                pot = 0,
                bets = resetBets,
                contributions = resetContributions,
                gamePhase = PreFlop,
                revealedCommunityCards = [],
                roundWinner = Nothing,
                roundWinnerHand = Nothing,
                dealerPos = newDealerPos,
                smallBlindIndex = newSmallBlindIndex,
                bigBlindIndex = newBigBlindIndex
            }

        gameLoop (currentRound + 1) maxRounds nextRoundGS



printPlayerChips :: Player -> IO ()
printPlayerChips p = putStrLn $ name p ++ " (Player #" ++ show (playerIndex p) ++ "): " ++ show (chips p) ++ " chips"

main :: IO ()
main = do

    let p1 = customPlayer "Andy" True 0
    let p2 = customPlayer "Beth" False 1
    let p3 = customPlayer "Charlie" False 2

    let initialPlayers = [p1, p2, p3]
    

    shuffledDeck <- shuffleDeck getDeck
    

    let gs = GameState {
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
            bigBlindIndex = 0,
            smallBlindIndex = 1
          }

    gameLoop 1 100 gs
