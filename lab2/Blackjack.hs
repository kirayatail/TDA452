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

value :: Hand -> Integer
value h = let (val, aces) = value' h in
          let bigVal = val + aces * 11 in
          if bigVal < 21 then bigVal
          else val + aces


value' :: Hand ->  (Integer, Integer)
value' Empty = (0, 0)
value' (Add (Card Ace _) n) = tupleSum (0, 1) (value' n)
value' (Add (Card (Numeric num) _) n) = tupleSum (num, 0) (value' n)
value' (Add _ n) = tupleSum (10, 0) (value' n)

tupleSum:: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
tupleSum (a, b) (a', b') = (a + a', b + b')

gameOver:: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner playerHand bankHand
  | playerVal > 21 = Bank
  | playerVal > bankVal = Player
  | otherwise = Bank
    where playerVal = value playerHand
          bankVal = value bankHand
