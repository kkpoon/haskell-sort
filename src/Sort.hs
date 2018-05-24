module Sort
    ( quicksort
    , mergesort
    ) where

import Data.List (partition)


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = (quicksort left) ++ [x] ++ (quicksort right)
    where (left, right) = partition (<x) xs


mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
    merge (mergesort left) (mergesort right)
    where
        p = (length xs) `div` 2
        (left, right) = splitAt p xs


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge a [] = a
merge [] b = b
merge a b =
    if x < y then
        x : merge xs b
    else
        y : merge a ys
    where
        x = head a
        y = head b
        xs = tail a
        ys = tail b
