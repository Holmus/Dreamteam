-- BlackJack.HS
module BlackJack where
import Cards
import RunGame


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

-- Determines and returns the value of a hand by utilizing the valueCard for each card
value :: Hand -> Integer
value Empty = 0
value (Add card hand) | valueCard card == 11 && value hand > 10 = value hand + 1
                      | otherwise = valueCard card + value hand

-- Determines and returns the number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) | valueCard card == 11 = 1 + numberOfAces hand
                             | otherwise = numberOfAces hand

-- Determines whether the hand is bust and returns true/false given that is the case or not
gameOver :: Hand -> Bool
gameOver Empty = False
gameOver (Add card hand) | value hand <= 21 = False
                         | numberOfAces hand == 0 = True -- Value of hand is >21 here
                         | otherwise = False -- The hand can't be a non-allowed combination of cards, because of aces

-- Given the guest hand and the bank hand, determines and returns the winner
winner :: Hand -> Hand -> Player
winner gHand bHand | gameOver gHand = Bank
                   | gameOver bHand = Guest
                   | value bHand >= value gHand = Bank
                   | otherwise = Guest

hand0 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)
hand1 = Add (Card Ace Hearts)
            (Add (Card Ace Spades) Empty)

-- Given two hands <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) Empty (Add c2 h2) = h2
(<+) (Add c1 h1) Empty = h1
(<+) (Add c1 h1) h2 = (<+) h1 (Add c1 h2)
