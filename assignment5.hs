{- Kaitlyn Cason
 - UC ID: 20282205
 - CS 1X1 Summer 16
 - Assignment 5
 - 3 September 2016
 -}

--Question 1
insert :: (Ord a) => [a] -> a -> [a]
insert [] y = [y]
insert (x:xs) y = if x < y then x : insert xs y else y : x : xs

--Question 2
insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insert (insertSort xs) x

--Question 3
merge :: (Ord a) => [[a]] -> [a]
merge xss = foldr (mergeHelp) [] xss

mergeHelp :: (Ord a) => [a] -> [a] -> [a]
mergeHelp xs ys = foldr (flip(insert)) xs ys

--Question 4
--Note: If odd number of fill instances, put the odd number on the left side
center :: (Ord a) => [a] -> Int -> a -> [a]
center lst len fill | length(lst) >= len = lst
					| length(lst)+1 == len = fill:lst
					| otherwise = [fill] ++ (center lst (len-2) fill) ++ [fill]

--Question 5
largest :: (Ord a) => [a] -> a
largest = foldl1 (max)
