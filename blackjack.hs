{-
Blackjack Game
CPSC 312 Project 1
Victor Cheng, Janice Wu, Adrian Pikor


Very simple implentation of Blackjack/Twenty-One:
	1) 2 players, human and computer (dealer)
	2) Each player keeps drawing cards (face-up, i.e. visible) one at a time
	3) If any player goes over 21 (sum of their card values), they lose
-}



{-----------IMPORTS-----------}
--import System.Random
import System.IO
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Text.Read

{-----------DATA TYPES-----------}

-- ***the following data types are adapted from Dr. David Poole's "MagicSum.hs" example:

-- state consists of internal state and whether a player can Hit again or not
data State = State InternalState Bool Bool
         deriving (Ord, Eq, Show)

{-
internal state consists of a 4-tuple with values:
	- player's list of cards drawn
	- CPU's list of cards drawn
	- list of cards available to draw
	- boolean indicating current player (True = player, False = CPU)
-}
type InternalState = ([Card], [Card], [Card], Bool)

-- (suit, value (1 - 13, where ace = 1, jack = 11, queen = 12, king = 13)
type Card = (Char, Int)

type Player = State -> Action

type Game = Action -> State -> Result

-- result of a game is True if player won, False if computer won
data Result = EndOfGame Bool State
            | ContinueGame State
         deriving (Eq, Show)

{-
Actions:
		- Hit n = take another card (of index n, to be used by an external RNG)
		- Stand = take no more cards for the round
-}
data Action = Hit Int
             | Stand
         deriving (Ord, Eq, Show)



{-----------GAME FUNCTIONS-----------}

blackjack :: Game
-- if no available actions for either player, do nothing and continue the game
blackjack act (State (pCards, cCards, deck, currPlayer) False True) =
    ContinueGame (State (pCards, cCards, deck, not currPlayer) False True)
blackjack act (State (pCards, cCards, deck, currPlayer) True False) =
    ContinueGame (State (pCards, cCards, deck, not currPlayer) True False)

-- action = hit (needs to use checkSum to check the sum of a player's card values after a card was drawn,
--				 and drawFromDeck to draw a card from the deck)
blackjack (Hit n) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) 
    | currPlayer = checkSum (State (newCard:pCards, cCards, newDeck, not currPlayer) pCanHit cCanHit)
    | otherwise = checkSum (State (pCards, newCard:cCards, newDeck, not currPlayer) pCanHit cCanHit)
        where
             (newCard,newDeck) = drawFromDeck deck n

-- action = stand (sets player's boolean flag (pCanHit/cCanHit) to False)
blackjack (Stand) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)
    | currPlayer = ContinueGame (State (pCards, cCards, deck, currPlayer) False cCanHit)
    | otherwise = ContinueGame (State (pCards, cCards, deck, currPlayer) pCanHit False)


{-----------HELPER FUNCTIONS-----------}

-- returns card drawn and new deck of cards (nth card is drawn from deck)
drawFromDeck :: [Card] -> Int -> (Card, [Card])
drawFromDeck deck n = (deck !! n, [card | card <- deck, card /= (deck !! n)])

-- adds up values of a list of cards
sumCards :: [Card] -> Int
sumCards [] = 0
sumCards ((_,val):tCard) = val + (sumCards tCard)

-- checks whether a player has exceeded 21
checkSum :: State -> Result
checkSum (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)
    | sumCards pCards > 21 = EndOfGame False newGame
    | sumCards cCards > 21 = EndOfGame True newGame
    | otherwise = ContinueGame (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)

-- averages the cards in a deck 
avrg :: [Card] -> Int
avrg deck = sumCards deck `div` length deck

-- called in aiBetting
-- decision making of the ai , takes in current state and average of the deck 
-- will produce one of the following values 
--     0  == stand and don't bet
--     1 == stand and bet 
--     2 == draw and don't bet 
--     3 == draw and bet 
aiDecide (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) avg =
    do
        let aiHand = sumCards cCards
        let pHand = sumCards pCards
        let missing = 21.0 - fromIntegral aiHand
        let cMiss = 21.0 - fromIntegral pHand
        if(missing < avg) 
            then 
            if(pHand < aiHand)
                then
                    return 1
                    else 
                    return 0
                else
                    if(pHand < aiHand)
                        then
                        return 3
            else 
                return 2

-- requires checksum to be called before 
-- decides how much ai will bet based on aiDecide and how much money is currently in hand
-- returns the amount money ai will bet
aiBet (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) avg money decision =
    do
        let aiHand = sumCards cCards
        let missing = 21 - fromIntegral aiHand
        let percent = fromIntegral aiHand `div` 21.0
        if(decision == 0 || decision == 2)
            then
            return 0
                else
                if(decision == 1)
                    then 
                    if(percent > 0.66)
                        then
                        return percent * money
                            else 
                            return percent * money / 2
                        else
                            return percent * money / 3

{-----------CONSTANTS/STARTING STATES-----------}
-- whole 52 card deck
--	's' == spades
--	'd' == diamonds
--	'h' == hearts
--	'c' == clubs
fullDeck :: [Card]
fullDeck = [(suit,value) | suit <- ['s','d','h','c'], value <- [1..13]]

-- start of new blackjack game
newGame :: State
newGame = State ([], [], fullDeck, True) True True

{-----------USER INTERFACE-----------}
type Bet = (Int, Int) --User's money, AI's money
start :: Game -> State -> IO Bet
start game state = 
    do
        putStrLn ("Game start! Welcome To BlackJack. Please enter the amount of money that you want to spend.")
        line <- getLine
        if  (all isDigit line)
            then
                --let x = 123
                --return (read line :: Int, read line :: Int)
                play game state (read line :: Int, read line :: Int)
            else
              do
                putStrLn ("Not an Integer, return (0,0) ")
                --line <- getLine
                return (0, 0)
play :: Game -> State -> Bet -> IO Bet
play game state (umoney, aimoney) = 
    do
      putStrLn ("New game - Who Starts? Quit = 1, You = 2, AI = 3")
      line <- getLine
      if line == "1"
         then
             do
                putStrLn ("Done! Money Left - User: " ++ show umoney ++ " AI: " ++ show aimoney)
                return (umoney, aimoney)
      else if line == "2"
         then 
            person_play game (game (HIT 2) state) (umoney, aimoney) 0
      else
            return (12,12)

-- User decision
--     0  == stand and don't bet
--     1 == stand and bet 
--     2 == draw and don't bet 
--     3 == draw and bet 
--person_play :: Game -> Result -> Bet -> IO Bet
person_play game (ContinueGame state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) = state in
    do
        putStrLn ("State - User's card: " ++ show pCards ++ ", AI's card: " ++ show cCards)
        putStrLn ("User Hit: " ++ show pCanHit ++ ", AI HIT: " ++ show cCanHit)
        putStrLn ("Money Left - User: " ++ show umoney ++ " AI: " ++ show aimoney)
        putStrLn ("How much you want to bet?")
        line <- getLine
        if  (all isDigit line)
           then
               do
                    putStrLn ("You bet: " ++ show line ++ ", Hit:1 or flow: else")
                    line <- getLine
                    if line == "1"
                      then 
                      AI_play game (game (Hit 1) state) (umoney, aimoney) 0+value
                    else
                      AI_play game (game (Stand) state) (umoney, aimoney) value
        else
            person_play game (ContinueGame state) (umoney, aimoney) value

person_play game (EndOfGame player state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) = state in
    do
       result <- update_bet (umoney, aimoney) False value
       play game state result


AI_play game (EndOfGame player state) (umoney, aimoney) value =
    do
       result <- update_bet (umoney, aimoney) True value
       play game state result
	   
AI_play:: Game -> Result -> Bet -> Num -> 
AI_play game (ContinueGame state) (umoney, aimoney) value =
    person_play game (game (Hit 1) state) result value
	
	
update_bet (umoney, aimoney) bool value = 
    if bool == True
       then (umoney + value, aimoney - value)
    else
	   (umoney - value, aimoney + value)

{-----------Start the program-----------}
--start blackjack newGame

















