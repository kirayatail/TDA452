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

hand2 = Add (Card Jack Spades) (Add (Card King Hearts) Empty)
hand3 = Add (Card Ace Spades) (Add (Card Ace Hearts) (Add (Card (Numeric 3) Hearts) Empty))

empty :: Hand
empty = Empty

-- Calculates the value of a hand taking the rules about aces into account.
value :: Hand -> Integer
value h | highVal <= 21 = highVal
        | otherwise    = lowVal
        where (lowVal, aces) = lowValueAndAces h
              highVal = lowVal + aces * 10

-- Traverses a hand and returns the value where aces count as 1 and
-- the amount of aces.
lowValueAndAces :: Hand ->  (Integer, Integer)
lowValueAndAces Empty                          = (0, 0)
lowValueAndAces (Add (Card Ace _) n)           = tupleSum (1, 1)(lowValueAndAces n)
lowValueAndAces (Add (Card (Numeric num) _) n) = tupleSum (num, 0) (lowValueAndAces n)
lowValueAndAces (Add _ n)                      = tupleSum (10, 0) (lowValueAndAces n)

-- Function for summing corresponding members of two tuples
tupleSum:: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
tupleSum (a, b) (a', b') = (a + a', b + b')

-- bust means value > 21
gameOver:: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner playerHand bankHand
  | playerVal > 21 = Bank
  | playerVal > bankVal = Player
  | otherwise = Bank
    where playerVal = value playerHand
          bankVal = value bankHand
