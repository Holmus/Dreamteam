-- By Oscar Evertsson and Jakob Holmgren

import Test.QuickCheck

-- Part 1:

--K + 1 steps are being used.

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--Part 2:

power1 :: Integer -> Integer -> Integer
power1 n 0 = 1
power1 n k | k < 0 = error "power: negative argument"
           | otherwise = product(replicate (fromInteger k) n)

--Part 3: 

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative argument"
           | even k = power2 (n*n) (k `div` 2)
           | otherwise = n * power2 n (k-1)

--Part 4

{-A

The function is defined for k >= 0 and for any value on n. This is why we won't test the cases when k < 0.

T1: 0 0, is an edge.
T2: 10 0, is also and edge case for k
T3: -100 2, testing something with a possitive result
T4: -100 3, testing something with a negative result
T5: 10 4, typical normal value
T6: 100000 9999999, is testing a large value 

B
-}

prop_power :: Integer -> Integer -> Bool
prop_power n k = (power n k == power2 n k) && (power n k == power1 n k) 

--C

test = (prop_power 0 0) &&
       (prop_power 10 0) && 
       (prop_power (-100) 2) && 
       (prop_power (-100) 3) &&
       (prop_power 10 4) &&
       (prop_power 100000 9999999)
--D
prop_power' :: Integer -> Integer -> Bool
prop_power' n k = prop_power n (abs k) -- Since the function only is defined for k >= 0 we use abs. 


