data Order = Online Float Int Int | Offline Float
isOnline :: Order -> Bool
isOnline (Online _ _ _) = True
isOnline (Offline _) = False

timeUntilReceiving :: Order -> Int
timeUntilReceiving (Online _ _ x) = x

totalPrice :: [Order] -> Float
totalPrice [] = 0
totalPrice ((Online x _ _):ys) = x + totalPrice ys
totalPrice ((Offline x):ys) = x + totalPrice ys

onlineOrders :: [Order] -> Int
onlineOrders = length . filter isOnline

isExpensive :: Order -> Bool
isExpensive x = 100 < totalPrice [x]

instance Show Order where
    show (Online price num time) = show price ++ " " ++ show num ++ " " ++ show time
    show (Offline price) = show price

instance Eq Order where
    (==) (Online price num time) (Online price1 num1 time1) = price == price1 && num == num1 && time == time1
    (==) (Offline price) (Offline price1) = price == price1
    (==) _ _ = False

data Tree a = Empty | Node a (Tree a) (Tree a)

levelSum :: Tree Int -> Int -> Int
levelSum Empty _               = 0
levelSum (Node a _ _) 0        = a
levelSum (Node a left right) x = (levelSum left (x-1)) + (levelSum right (x-1))

cone :: Tree Int -> Bool
cone Empty = True
cone t = ascending . sumList t $ (height t - 1)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

sumList :: Tree Int -> Int -> [Int]
sumList _ (-1) = []
sumList t n = sumList t (n-1) ++ [levelSum t n]

ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending (x:[]) = True
ascending (x:y:zs) = x < y && ascending (y:zs)
