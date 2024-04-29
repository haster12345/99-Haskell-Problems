myLast :: [a] -> a
myLast [] = error "List is empty, last element does not exists"
myLast [x] = x
myLast (_ : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt x b = x !! (b - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse list = reverse' list []
  where
    reverse' [] reversed = reversed
    reverse' (x : xs) reversed = reverse' xs (x : reversed)
