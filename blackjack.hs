{-
Blackjack Game
CPSC 312 Project 1
Victor Cheng, Janice Wu, Adrian Pikor


Very simple implentation of Blackjack/Twenty-One:
	1) 2 players, human and computer (dealer)
	2) Each player keeps drawing cards (face-up, i.e. visible) one at a time
	3) If any player goes over 21 (sum of their card values), they lose
	4) Card values:
		- Ace: 1 or 11
		- 2 - 10: same value as number
		- Jack, Queen, King: 10
-}



{-----------DATA TYPES-----------}

-- ***the following data types are adapted from Dr. David Poole's "MagicSum.hs" example:

-- state consists of internal state and possible actions each player can take
-- (list for player's possible actions, list for CPU's possible actions)
data State = State InternalState [Action] [Action]
         deriving (Ord, Eq, Show)

{-
internal state consists of a 4-tuple with values:
	- player pair:
		- list of cards drawn
		- bet amount
	- CPU pair:
		- list of cards drawn
		- bet amount
	- list of cards available to draw
	- boolean indicating current player (True = player, False = CPU)
-}
type InternalState = ([Card], [Card], [Card], Bool)

type Card = (Char, Integer)

type Player = State -> Action

type Game = Action -> State -> Result

-- result of a game is True if player won, False if computer won
data Result = EndOfGame Bool State
            | ContinueGame State
         deriving (Eq, Show)

{-
Actions:
		- Hit = take another card
		- Stand = take no more cards for the round
-}
data Action = Hit
			| Stand
         deriving (Ord, Eq, Show)



{-----------GAME FUNCTIONS-----------}

blackjack :: Game
-- if no available actions for either player, do nothing and continue the game
blackjack act (State (pCards, cCards, deck, currPlayer) [] cAvailAct) =
	ContinueGame (State (pCards, cCards, deck, not currPlayer) [] cAvailAct)
blackjack act (State (pCards, cCards, deck, currPlayer) pAvailAct []) =
	ContinueGame (State (pCards, cCards, deck, not currPlayer) pAvailAct [])

-- action = hit (needs to use checkSum to check the sum of a player's card values after a card was drawn)
--blackjack (Hit) (State (pState, cState, deck, currPlayer) pAvailAct cAvailAct) =
--	ContinueGame (drawFromDeck (State (pState, cState, deck, currPlayer) pAvailAct cAvailAct))

-- action = stand
blackjack (Stand) (State (pCards, cCards, deck, currPlayer) pAvailAct cAvailAct)
	| currPlayer = ContinueGame (State (pCards, cCards, deck, currPlayer) [] cAvailAct)
	| otherwise = ContinueGame (State (pCards, cCards, deck, currPlayer) pAvailAct [])


{-----------HELPER FUNCTIONS-----------}

--drawFromDeck :: State -> State
--NEED TO DEFINE

-- adds up values of a list of cards
sumCards :: [Card] -> Integer
sumCards [] = 0
sumCards ((_,val):tCard) = val + (sumCards tCard)

-- checks whether a player has exceeded 21
checkSum :: State -> Result
checkSum (State (pCards, cCards, deck, currPlayer) pAvailAct cAvailAct)
	| sumCards pCards > 21 = EndOfGame False newGame
	| sumCards cCards > 21 = EndOfGame True newGame
	| otherwise = ContinueGame (State (pCards, cCards, deck, currPlayer) pAvailAct cAvailAct)



{-----------CONSTANTS/STARTING STATES-----------}

-- whole 52 card deck enumerated
--	's' == spades
--	'd' == diamonds
--	'h' == hearts
--	'c' == clubs
fullDeck :: [Card]
fullDeck = [('s',1), ('s',2), ('s',3), ('s',4), ('s',5), ('s',6), ('s',7),
	('s',8), ('s',9), ('s',10), ('s',11), ('s',12), ('s',13), --spades
	('d',1), ('d',2), ('d',3), ('d',4), ('d',5), ('d',6), ('d',7),
	('d',8), ('d',9), ('d',10), ('d',11), ('d',12), ('d',13), --diamonds
	('h',1), ('h',2), ('h',3), ('h',4), ('h',5), ('h',6), ('h',7),
	('h',8), ('h',9), ('h',10), ('h',11), ('h',12), ('h',13), --hearts
	('c',1), ('c',2), ('c',3), ('c',4), ('c',5), ('c',6), ('c',7),
	('c',8), ('c',9), ('c',10), ('c',11), ('c',12), ('c',13)] --clubs

-- start of new blackjack game
newGame :: State
newGame = State ([], [], fullDeck, True) [Hit, Stand] [Hit, Stand]