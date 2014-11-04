-- Lab Assignment 1
import Data.List
import Test.QuickCheck

-- Calculate n^k recursively.
power :: Integer -> Integer -> Integer
power n k | k < 0	= error "power: negative exponent"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1

-- 'power n k' takes k+1 steps for all k >= 0

-- Part 2

-- Calculate n^k by calling the product of a list with n repeated times.
power1 :: Integer -> Integer -> Integer
power1 n k | k >= 0 = product (genericReplicate k n)
           | otherwise = error "power1: negative exponent"

-- Part 3

-- Calculate n^k by splitting k in half when it is even.
power2 :: Integer -> Integer -> Integer
power2 n 0             = 1
power2 n k | k < 0     = error "power2: negative exponent"
           | even k    = power2 (n * n) (div k 2)
           | otherwise = n * power2 n (k-1)

-- Part 4

-- A

{-
  We believe the following test cases are important:
  * k == 0 - to ensure that the base case is handled correctly,
    result should always be 1
  * k < 0 - Avoid testing this property, as it's not defined for the function
  * k > 0 - to ensure that positive exponents are handled correctly
  * even k - since one of the function treats odd and even numbers
    differently
  * odd k - since one of the function treats odd and even numbers differently
  * n == 0 - should be 0 for all k except k == 0 where it should be 1
  * n < 0 - should handle negative n
  * n > 0
-}


-- B

-- Property that checks that power, power1 and power2 produce equal results.
-- Negative values for k are avoided by only using the absolute value.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = let k' = abs k in
                  power2 n k' == power n k' &&
                  power1 n k' == power n k'


-- C

--  Cartesian product for n X k as mentioned in part A.
prop_powersAll :: Integer -> Integer -> Bool
prop_powersAll n k = let ns = [n, 0, -n] in
                     let ks = [0, abs k, 1 + abs k] in
                     and [prop_powers n' k' | n' <- ns, k' <- ks]


-- D

{-
  We avoided getting errors from our power functions since we decided not
  to test negative k values in prop_powers by using the absolute value.
-}
