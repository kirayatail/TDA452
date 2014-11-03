-- Lab Assignment 1

-- Calculate n^k recursively.
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1

-- It takes k + 1 steps.

-- Part 2

-- Calculate n^k by calling calculating the product of a list with n repeated
-- times.
power1 :: Integer -> Integer -> Integer
power1 n k | k >= 0    = product [n | x <- [1..k]]
           | otherwise = error "power with negative exponent not allowed"

-- Part 3

-- Calculate n^k by splitting k in half when it is even.
power2 :: Integer -> Integer -> Integer
power2 n 0             = 1
power2 n k | k < 0     = error "power with negative exponent not allowed"
           | even k    = power2 (n*n) (div k 2)
           | otherwise = n * power2 n (k-1)
-- Part 4

-- A

{-
  We believe the following test cases are important:
  * k == 0 - to ensure that the base case is handled correctly
  * k < 0 - to ensure that an error is thrown with negative exponents
  * k > 0 - to ensure that positive exponents are handled correctly
  * even k - since one of the function treats odd and even numbers
    differently
  * odd k - since one of the function treats odd and even numbers differently
  * n == 0
  * n < 0
  * n > 0
-}

-- B

-- C
