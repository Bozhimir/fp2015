containsDigits :: Int -> Int -> Bool
containsDigits a b
    | b < 10 = contains a b
    | otherwise = contains a (mod b 10) || containsDigits a (div b 10)
        where
            contains n x
                | n < 10    = n == x
                | otherwise = x == (mod n 10) || contains (div n 10) x
				
interestingNumber :: Int -> Bool
interestingNumber n
    | n < 1     = False
    | otherwise = n == sumDivsiors (sumDivisors n 1) 1
        where
            sumDivisors n curr
                | curr == n         = 0
                | (mod n curr) == 0 = curr + sumDivisors n (curr + 1)
                | otherwise         = sumDivisors n (curr + 1)
				
productOfDigits :: Int -> Int
productOfDigits n
    | n < 10    = n
    | otherwise = (mod n 10) * productOfDigits (div n 10)
	
quadrant :: Double -> Double -> Int
quadrant 0.0 0.0 = 0
quadrant x y
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | otherwise      = 4
