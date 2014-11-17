--  Lab 2 - A  Simple Black Jack Variant


-- Hand execution of size hand2

{-
  size hand2
  = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}

module BlackJack where
import Cards
import Wrapper
import System.Random

hand1 = Add (Card Jack Clubs) (Add (Card Ace Hearts) Empty)
hand2 = Add (Card Jack Spades) (Add (Card King Hearts) Empty)
hand3 = Add (Card Ace Spades) (Add (Card Ace Hearts) (Add (Card (Numeric 3) Hearts) Empty))

-- Create an empty hand.
empty :: Hand
empty = Empty

-- Determine the value of a hand, taking the special rules about aces into
-- account.
value:: Hand -> Integer
value h | highValue > 21 = lowValue
        | otherwise      = highValue
        where
          aces                 = numberOfAces h
          handValue Empty      = 0
          handValue (Add c h') = valueCard c + handValue h'
          lowValue             = handValue h
          highValue            = lowValue + aces * 10

-- Determine the value of a card.
valueCard:: Card -> Integer
valueCard (Card r _) = valueRank r

-- Determine the value  of a rank. Here aces count as 1.
valueRank:: Rank -> Integer
valueRank Ace         = 1
valueRank (Numeric n) = n
valueRank _           = 10

-- Count how many aces a hand contains.
numberOfAces:: Hand -> Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h)            = numberOfAces h

-- Determines if the game is over. This happens when one of the players go bust.
gameOver:: Hand -> Bool
gameOver h = value h > 21

-- Determines whether the bank or guest wins.
-- The implementation assumes only one of the players have gone bust as
-- specified in the rules.
winner :: Hand -> Hand -> Player
winner playerHand bankHand
  | gameOver playerHand = Bank
  | gameOver bankHand   = Guest
  | playerVal > bankVal = Guest
  | otherwise           = Bank
  where playerVal = value playerHand
        bankVal   = value bankHand

-- Puts the first hand on top of the second.
(<+) ::Hand -> Hand -> Hand
(<+) Empty bottom = bottom
(<+) (Add topCard topHand) bottom = Add topCard (topHand <+ bottom)

-- Ensure that <+ is assocoiative
prop_onTopOf_assoc ::Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc h1 h2 h3 = h1 <+ (h2 <+ h3) == (h1 <+ h2) <+ h3

-- Ensure that the size of two hands on top of each other is the same as
-- the size of the sum of the sizes of the separate hands.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 =  size h1  + size h2 == size (h1 <+ h2)

-- Generate a full deck (not shuffled)
fullDeck :: Hand
fullDeck = suit Spades <+
           suit Diamonds <+
           suit Clubs <+
           suit Hearts
  where
    ranks = [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace]
    suit = suitFromList ranks
    suitFromList (r:rs) s = Add (Card r s) (suitFromList rs s)
    suitFromList _ _ = Empty

-- Draw the top card of the deck and put it into a hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add top deck) hand = (deck, Add top hand)

-- The basic AI for the bank.
playBank :: Hand -> Hand
playBank deck = playBank' deck empty

-- Recursive part of playBank.
playBank' :: Hand -> Hand -> Hand
playBank' deck hand
  | value hand' < 16 = playBank' deck' hand'
  | otherwise        = hand'
  where (deck', hand') = draw deck hand

-- Wrapper function for the recursive shuffle
shuffle :: StdGen -> Hand -> Hand
shuffle g hand = shuffle' g hand Empty

-- Pick random cards from one hand and place it in the other
-- until first hand is empty.
shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' g Empty new = new
shuffle' g deck new = shuffle' g' deck' (Add card new)
  where
    (num, g') = randomR (0, size deck -1) g
    (card, deck') = pickCard deck Empty num

-- Go through the deck, return the card at place num
-- and the rest of the cards as one hand.
pickCard :: Hand -> Hand -> Integer -> (Card, Hand)
pickCard Empty Empty _          = error "pickCard: All hands are empty"
pickCard Empty (Add card top) _ = (card, top)
pickCard deck (Add card top) 0  = (card, top <+ deck)
pickCard deck top num           = pickCard deck' top' (num - 1)
  where (deck', top')           = draw deck top

-- Check if a card is in a hand.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Ensure that a card that belongs to a deck is still there after shuffling.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h

-- Ensure that the size of a hand is preserved after shuffling.
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

-- Implement the inteface for the game.
implementation = Interface
  {
    iEmpty    = empty,
    iFullDeck = fullDeck,
    iValue    = value,
    iGameOver = gameOver,
    iWinner   = winner,
    iDraw     = draw,
    iPlayBank = playBank,
    iShuffle  = shuffle
  }

-- Start a game of Black Jack.
main :: IO()
main = runGame implementation
