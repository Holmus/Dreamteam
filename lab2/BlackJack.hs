-- BlackJack.HS
module BlackJack where
import Cards
import RunGame
import System.Random

{-

Task 3.2

size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}

-- Task 3.3 start

main :: IO ()
main = runGame implementation

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Determines and returns the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _   = 10

-- Determines and returns the value of a card by utilizing the valueRank for each card
valueCard :: Card -> Integer
valueCard (Card rank suit) = valueRank rank

-- Calculates the value of a hand assuming aces = 11.
value' :: Hand -> Integer
value' Empty = 0
value' (Add card hand) = valueCard card + value' hand      

-- Determines and returns the value of a hand by utilizing the valueCard for each card
value :: Hand -> Integer
value hand | totVal > 21 = totVal - ((numberOfAces hand) * 10)
           | otherwise = totVal
           where totVal = value' hand

-- Determines and returns the number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace s) h) = 1 + numberOfAces h
numberOfAces (Add c h) = numberOfAces h

-- Determines whether the hand is bust and returns true/false given that is the case or not
gameOver :: Hand -> Bool
gameOver hand | value hand <= 21 = False
              | otherwise = True -- The hand can't be a non-allowed combination of cards, because of aces

-- Given the guest hand and the bank hand, determines and returns the winner
winner :: Hand -> Hand -> Player
winner gHand bHand | gameOver gHand = Bank
                   | gameOver bHand = Guest
                   | value bHand >= value gHand = Bank
                   | otherwise = Guest

-- Task 3.3 End

-- Given two hands <+ puts the first one on top of the second one

(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) Empty h2 = h2
(<+) h1 Empty = h1
(<+) (Add c1 h1) h2 = (Add c1 ((<+) h1 h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

--Furthermore the size of the combined hand should be the sum of the sizes of the two individual hands:
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size ((<+) h1 h2)

fullDeck :: Hand
fullDeck = foldr Add Empty
         [Card rank suit | 
         rank <- map Numeric [2..10]++[Jack,Queen,King,Ace], 
         suit <- [Hearts, Spades, Diamonds, Clubs]]

draw :: Hand -> Hand -> (Hand,Hand) 
draw Empty (Add c2 h2) = error "draw: The deck is empty."
draw (Add c1 h1) Empty = (h1, (Add c1 Empty))
draw (Add c1 h1) h2 = (h1, (Add c1 h2))

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand -- Deck, current hand and gives the final hand for the bank
playBank' (Add c1 h1) h2 | value h2 < 16 = playBank' hand1 hand2 
                         | otherwise = h2
                         where (hand1, hand2) = draw h1 h2

-- Takes the n:th card in the hand and returns it together with the remaining hand.
-- indexed from 1 instead of 0.
-- Only defined for n >= 1.

--hand1 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty)
takeNthCard :: Int -> Hand -> (Hand,Card)
takeNthCard 1 (Add c1 h1) = (h1, c1)
takeNthCard n (Add c1 h1) | n <= 0 = error "You can't pick a card on a negative positon"
                          | otherwise = (((<+) (Add c1 Empty) remainingHand), tempCard)
                          where (remainingHand, tempCard) = takeNthCard (n-1) h1

shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g hand = shuffle' g hand Empty

shuffle':: StdGen -> Hand -> Hand -> Hand
shuffle' g Empty h2 = h2
shuffle' g h1 h2 = shuffle' newG h3 (Add c1 h2)
    where (h3, c1) = takeNthCard value h1
          (value, newG) = randomR (1, size h1) g

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

