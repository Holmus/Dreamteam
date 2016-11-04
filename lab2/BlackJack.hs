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

hand2 = Add (Card Jack Hearts)
            (Add (Card (Numeric 5) Spades) Empty)

-- Given two hands <+ puts the first one on top of the second one

(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) Empty h2 = h2
(<+) h1 Empty = h1
(<+) (Add c1 h1) h2 = (Add c1 ((<+) h1 h2))  --(<+) h1 (Add c1 (Add c2 h2))   -- h1 (Add c1 h2) 

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
draw (Add c1 h1) (Add c2 h2) = (h1, (Add c1 h2))

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand -- Deck, current hand and gives the final hand for the bank
playBank' (Add c1 h1) (Add c2 h2) | value h2 < 16 = playBank' hand1 hand2 
								  | otherwise = h2
		where (hand1, hand2) = draw h1 h2



