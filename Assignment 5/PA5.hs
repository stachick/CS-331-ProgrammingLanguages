-- PA5.hs
-- Shane Tachick
-- March 31, 2015
-- CS 331
-- Assignment 5

module PA5 where

import qualified Data.List (isInfixOf)

-- The workhorse function for collatzCounts, does the recursive work based on the value
-- being even or odd. Terminates when it reaches 1
collatz 0 = 0
collatz 1 = 1
collatz n
  | even n = (n `div` 2)
  | otherwise = (3 * n + 1)

-- Takes a starting integer of a collatz sequence and produces the resulting list of
-- integers until 1 is reached
collSeq = index . iterate collatz
  where
    index (1:_) = []
    index (x:xs) = (x:(index xs))

-- Makes a list of the lengths of various collatz sequences
collatzCounts = map (length . collSeq) [1..]

-- Takes two lists and strips everything from list y starting from
-- the point where list x was found within list y
searchedList x y
  | Data.List.isInfixOf x (init y) = searchedList x (init y)
  | otherwise = y

-- Takes two lists and, using the stripped list y, finds where the
-- sublist x starts within it
findList x y
  | (length x == 0) = Just 0 
  | Data.List.isInfixOf x y = Just ((length $ searchedList x y) - length x)
  | otherwise = Nothing

-- Analyses 2 lists and returns the number of indexes that have the same element on both lists
l1 ## l2 = length . map fst $ filter (\(x,y) -> x == y) $ zip l1 l2