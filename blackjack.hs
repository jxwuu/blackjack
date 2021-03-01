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
import System.Random
import System.IO
import Control.Monad
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Text.Read
import Debug.Trace

{-----------DATA TYPES-----------}

debug = flip trace
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
            | Tie State
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
blackjack act (State (pCards, cCards, deck, True) False cCanHit) =
    ContinueGame (State (pCards, cCards, deck, False) False cCanHit)
blackjack act (State (pCards, cCards, deck, False) pCanHit False) =
    ContinueGame (State (pCards, cCards, deck, True) pCanHit False)

-- action = hit (needs to use checkSum to check the sum of a player's card values after a card was drawn,
--				 and drawFromDeck to draw a card from the deck)
blackjack (Hit n) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) 
    | currPlayer = checkSum (State (newCard:pCards, cCards, newDeck, currPlayer) pCanHit cCanHit)
    | otherwise = checkSum (State (pCards, newCard:cCards, newDeck, not currPlayer) pCanHit cCanHit)
        where
             (newCard,newDeck) = drawFromDeck deck n

-- action = stand (sets player's boolean flag (pCanHit/cCanHit) to False)
blackjack (Stand) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)
    | pCanHit == cCanHit && cCanHit == False = checkSum (State (pCards, cCards, deck, currPlayer) False False)
    | pCanHit == False || cCanHit == False = checkSum (State (pCards, cCards, deck, currPlayer) False False)
    | currPlayer = ContinueGame (State (pCards, cCards, deck, not currPlayer) False cCanHit)
    | otherwise = ContinueGame (State (pCards, cCards, deck, not currPlayer) pCanHit False)

 

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
    | pSum > 21 || (not pCanHit && not cCanHit && 21 - pSum > 21 - cSum) = EndOfGame False newGame
    | cSum > 21 || (not pCanHit && not cCanHit && 21 - pSum < 21 - cSum) = EndOfGame True newGame
    | (not pCanHit && not cCanHit && 21 - pSum == 21 - cSum) = Tie newGame
    | otherwise = ContinueGame (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)
      where
        pSum = sumCards pCards
        cSum = sumCards cCards

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
aiDecide :: (Ord a, Num a, Num p) => [Card] -> [Card] -> a -> p
aiDecide pHand cHand avg
    | (21 - fromIntegral (sumCards cHand)) < avg && ((sumCards pHand)-2) < (sumCards cHand)*4 `div` 3  = 1
    | (21 - fromIntegral (sumCards cHand)) < avg && ((sumCards pHand)-2) > (sumCards cHand) = 0
    | (21 - fromIntegral (sumCards cHand)) >= avg && (sumCards pHand) >= (sumCards cHand)*4 `div` 3 = 2
    | otherwise = 3

-- requires checksum to be called before 
-- decides how much ai will bet based on aiDecide and how much money is currently in hand
-- returns the amount money ai will bet
aiBet cHand avg money decision 
    | decision == 0 = 0 
    | decision == 2 = 0
    | decision == 1 &&  (sumCards cHand) `div` 21 * 100 > 66 = (sumCards cHand) * money `div` 100
    | decision == 1 &&   (sumCards cHand) `div` 21 * 100 <= 66 =  (sumCards cHand) * money `div` 200
    | otherwise = (sumCards cHand) * money `div` 300


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
  let (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) = state in
    do
      putStrLn ("New game - Who Starts? Quit = 1, You = 2, AI = 3")
      line <- getLine
      num <- randomRIO (0, (length deck) - 1) :: IO Int
      if line == "1" then
        do
          putStrLn ("Done! Money Left - User: " ++ show umoney ++ " AI: " ++ show aimoney)
          return (umoney, aimoney)
      else if line == "2" then 
          person_play game (game (Hit num) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)) (umoney, aimoney) 0
      else
          ai_play game (game (Hit num) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)) (umoney, aimoney) 0

-- User decision
--     0  == stand and don't bet
--     1 == stand and bet 
--     2 == draw and don't bet 
--     3 == draw and bet 
person_play :: Game -> Result -> Bet -> Int -> IO Bet
person_play game (ContinueGame state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) = state in
      do
        putStrLn ("Your hand:        " ++ show pCards)
        putStrLn ("Computer's hand:  " ++ show cCards)
        -- putStrLn ("User Hit: " ++ show pCanHit ++ ", AI HIT: " ++ show cCanHit)
        putStrLn ("Your money:        " ++ show umoney)
        putStrLn ("Computer's money:  " ++ show aimoney)
        putStrLn ("Current pool: " ++ show value)
        num <- randomRIO (0, (length deck) - 1) :: IO Int
        if pCanHit == False
            then
                ai_play game (game (Stand) state) (umoney, aimoney) value
        else
            do
                putStrLn ("How much do you want to bet?")
                line <- moneyHandle umoney
                if  (1 == 1)
                    then
                       let x = line :: Int in
                         do
                            putStrLn ("Hit = \"1\", Stand = any other key")
                            line <- getLine
                            if line == "1"
                              then 
                                 --ai_play game (game (Hit 1) state) (umoney - x, aimoney) $x+value
                                 ai_play game (game (Hit num) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)) (umoney - x, aimoney) $x+value --`debug` ( show $ state)
                            else
                                 ai_play game (game (Stand) state) (umoney - x, aimoney) $x+value --`debug` ( show $ state)
                else
                    person_play game (ContinueGame state) (umoney, aimoney) value

person_play game (EndOfGame player state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) = state in
    do
       result <- update_bet (umoney, aimoney) False value
       play game state result

person_play game (Tie state) (umoney, aimoney) value = 
    do
       putStrLn ("Tie!")
       play game newGame (umoney, aimoney)

ai_play:: Game -> Result -> Bet -> Int -> IO Bet
ai_play game (ContinueGame state) (umoney, aimoney) value =
    let (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit) = state in
    do
      putStrLn ("ai_play game (ContinueGame state) ") 
      let aiDecision = aiDecide pCards cCards (avrg deck)
      let computerBet = aiBet cCards (avrg deck) aimoney aiDecision 
      num <- randomRIO (0, (length deck) - 1) :: IO Int
      if aiDecision == 0 --stand and don't bet
        then
            do
              putStrLn ("AI stand but don't bet");
              putStrLn("--------------------------");
              person_play game (game Stand state) (umoney, aimoney) value
      else if aiDecision == 1 --stand and bet 
        then
            do
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              person_play game (game Stand state) (umoney, aimoney - computerBet) (value + computerBet) 
      else if aiDecision == 2  -- draw and don't bet 
        then
            do
              putStrLn ("AI draw but don't bet");
              putStrLn("--------------------------");
              person_play game (game (Hit num) (State (pCards, cCards, deck, False) pCanHit cCanHit)) (umoney, aimoney) value 
      else --draw and bet
         do
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              person_play game (game (Hit num) (State (pCards, cCards, deck, currPlayer) pCanHit cCanHit)) (umoney, aimoney - computerBet) (value + computerBet)
 
ai_play game (EndOfGame player state) (umoney, aimoney) value =
    do
       result <- update_bet (umoney, aimoney) True value
       play game state result

ai_play game (Tie state) (umoney, aimoney) value =
    do
       putStrLn ("Tie!")
       play game newGame (umoney, aimoney)
 
update_bet (umoney, aimoney) bool value
   | bool = do 
      putStrLn ("You Won")
      return (umoney + value, aimoney)
   | otherwise = do
      putStrLn ("AI Won")
      return (umoney, aimoney + value)

moneyHandle :: Int -> IO Int
moneyHandle y = do
  input1 <- getLine
  case readMaybe input1 of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      moneyHandle y
    Just n -> do
      if(n <= y)
       then
        return n
      else 
       do
        putStrLn "You don't have enough, please enter a lower number"
        moneyHandle y

{-----------Start the program-----------}
--start blackjack newGame

















