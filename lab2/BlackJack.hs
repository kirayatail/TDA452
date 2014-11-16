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
  | playerVal > bankVal = Guest
  | otherwise           = Bank
  where playerVal = value playerHand
        bankVal   = value bankHand

-- Puts the first hand on top of the second.
(<+) ::Hand -> Hand -> Hand
(<+) Empty bottom = bottom
(<+) (Add topCard topHand) bottom = Add topCard (topHand <+ bottom)

-- Generate a full deck (not shuffled)
fullDeck :: Hand
fullDeck = suitHand ranks Spades <+ suitHand ranks Diamonds <+
           suitHand ranks Clubs <+ suitHand ranks Hearts
  where
    ranks = [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace]
    suitHand :: [Rank] -> Suit -> Hand
    suitHand (r:rs) s = Add (Card r s) (suitHand rs s)
    suitHand _ s = Empty


-- Ensure that <+ is assocoiative
prop_onTopOf_assoc ::Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc h1 h2 h3 = h1 <+ (h2 <+ h3) == (h1 <+ h2) <+ h3

-- Ensure that the size of two hands on top of each other is the same as
-- the size of the sum of the sizes of the separate hands.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 =  size h1  + size h2 == size (h1 <+ h2)
