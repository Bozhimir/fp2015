count :: Eq a => a -> [a] -> Int
count el xs = length . filter (== el) xs 