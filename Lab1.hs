-- Part 1

	{-
	'power n k' takes k+1 steps for all k >= 0

	-}

-- Part 2

-- Generate a list of k elements with value n, apply the product function to receive power
--power1 :: Integral -> Integral -> Integer
power1 n k | k >= 0    = product [n | x <- [1..k]]
		   | otherwise = error "Exponent less than 0"

-- Part 3

power2 :: (Num a, Integral b) => a -> b -> a
power2 n 0					= 1
power2 n k  | k < 0			= error "Exponent less than 0"
			| even k		= power2 (n * n) (div k 2)
			| otherwise 	= n * power2 n (k-1)

-- prop_power2 