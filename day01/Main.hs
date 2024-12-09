module Main where

import System.Environment (getArgs)
import Data.Array (Array, accumArray, bounds, inRange, (!))
import Data.List (sort)

mapLinesToIntLists :: [String] -> ([Int], [Int])
mapLinesToIntLists [] = ([], [])
mapLinesToIntLists (line:rest) = (left : mappedLeft, right : mappedRight)
    where 
        [leftStr, rightStr] = words line
        left = read leftStr :: Int
        right = read rightStr :: Int
        (mappedLeft, mappedRight) = mapLinesToIntLists rest

main :: IO()
main = do
    args <- getArgs
    let doSorted = "--sorted" `elem` args
    contents <- readFile "input.txt"
    let (l1, l2) = mapLinesToIntLists (lines contents)
    if doSorted then handleSorted l1 l2 else handleUnsorted l1 l2

handleSorted :: [Int] -> [Int] -> IO()
handleSorted l1 l2 = do
    let (list1, list2) = (sort l1, sort l2)
        countArray = accumArray (+) 0 (head list2, last list2) [(x, 1) | x <- list2]   
    putStrLn ("Part 1 answer: " ++ show (part1Sorted list1 list2) :: String)
    putStrLn ("Part 2 answer: " ++ show (part2Sorted list1 list2 countArray) :: String)

handleUnsorted :: [Int] -> [Int] -> IO()
handleUnsorted list1 list2 = do
    putStrLn ("Part 1 answer: " ++ show (part1Unsorted list1 list2) :: String)
    putStrLn ("Part 2 answer: " ++ show (part2Unsorted list1 list2) :: String)

-- Sorted --

part1Sorted :: [Int] -> [Int] -> Int
part1Sorted [] _ = 0
part1Sorted _ [] = 0
part1Sorted (e1:rest1) (e2:rest2) = abs (e1 - e2) + part1Sorted rest1 rest2

part2Sorted :: [Int] -> [Int] -> Array Int Int -> Int
part2Sorted [] _ _ = 0
part2Sorted _ [] _ = 0
part2Sorted (e:rest) list2 counts = score + part2Sorted rest list2 counts
    where isElementInRange = inRange (bounds counts) e
          score = if isElementInRange then e * (counts ! e) else 0

-- Unsorted --

removeFirst :: [Int] -> Int -> [Int]
removeFirst (e:rest) element
    | e == element = rest
    | otherwise    = e : removeFirst rest element

count :: [Int] -> Int -> Int
count [] _ = 0
count (e:rest) element = (if e == element then 1 else 0) + count rest element

part1Unsorted :: [Int] -> [Int] -> Int
part1Unsorted [] _ = 0
part1Unsorted _ [] = 0
part1Unsorted l1 l2 = abs (one - two) + part1Unsorted (l1 `removeFirst` one) (l2 `removeFirst` two)
    where 
        one = minimum l1
        two = minimum l2

part2Unsorted :: [Int] -> [Int] -> Int
part2Unsorted [] _ = 0
part2Unsorted _ [] = 0
part2Unsorted (e:rest) list2 = e * (list2 `count` e) + part2Unsorted rest list2
