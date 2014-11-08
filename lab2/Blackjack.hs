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

tupleSum:: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
tupleSum (a, b) (a', b') = (a + a', b + b')
