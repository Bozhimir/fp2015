removeAt :: Int -> [a] -> [a]
removeAt x (y:ys)
    | x < 0         = error "Wrong Index!"
    | x > length ys = error "Wrong Index!"
    | x == 0        = ys
    | otherwise     = y : removeAt (x - 1) ys
	
suffix :: (Eq a) => [a] -> [a] -> Bool
suffix [] _ = True
suffix _ [] = False
suffix xs ys = (last xs) == (last ys) && suffix (init xs) (init ys)