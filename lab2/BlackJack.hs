--  Lab 2 - A  Simple Black Jack Variant

--Part 2

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
value h | handValue h + (10 * numberOfAces h) > 21 = handValue h
        | otherwise = handValue h + (10 * numberOfAces h)
        where handValue Empty = 0
              handValue (Add c h') = valueCard c + handValue h'

-- Determine the value of a card.
valueCard:: Card -> Integer
valueCard (Card r _) = valueRank r

-- Determine the value  of a rank. Here aces count as 1.
valueRank:: Rank -> Integer
valueRank Ace = 1
valueRank (Numeric n) = n
valueRank _ = 10

-- Count how many aces a hand contains.
numberOfAces:: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h) = numberOfAces h

-- bust means value > 21
gameOver:: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner playerHand bankHand
  | playerVal > 21 = Bank
  | playerVal > bankVal = Guest
  | otherwise = Bank
    where playerVal = value playerHand
          bankVal = value bankHand
