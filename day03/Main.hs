module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

main :: IO()
main = do
    contents <- readFile "input.txt"
    let seqProducts = partOne contents
        seqConditionalProducts = partTwo contents True

    putStrLn ("Part 1 answer: " ++ show (sum seqProducts) :: String)
    putStrLn ("Part 2 answer: " ++ show (sum seqConditionalProducts) :: String)

-- Part 1 --

-- Finds the `mul(x,y)` sequence in a String and returns all products in a list
partOne :: String -> [Int]
partOne [] = []
partOne string = case findSequence string of
    (Nothing, _) -> []
    (Just num, rest) -> num : partOne rest

extractNumber :: String -> Char -> (Maybe Int, String)
extractNumber str stop = case dropWhile isDigit str of 
        [] -> (Nothing, [])
        (s:rest) -> 
            if s == stop && num /= []
            then (Just (read num :: Int), rest) 
            else (Nothing, s:rest)
            where num = takeWhile isDigit str

findSequence :: String -> (Maybe Int, String)
findSequence str = case str of
    [] -> (Nothing, [])
    ('m':'u':'l':'(':rest) -> 
        case extractNumber rest ',' of
            (Nothing, ret1) -> findSequence ret1
            (Just x, ret1) -> 
                case extractNumber ret1 ')' of
                    (Nothing, ret2) -> findSequence ret2
                    (Just y, ret2) -> (Just (x * y), ret2)
    (s:rest) -> findSequence rest

-- Part 2 --

partTwo :: String -> Bool -> [Int]
partTwo string enabled = case findSequenceConditional string enabled of
    (Nothing, _, _) -> []
    (Just num, enabled, rest) -> num : partTwo rest enabled

findSequenceConditional :: String -> Bool -> (Maybe Int, Bool, String)
findSequenceConditional [] enabled = (Nothing, enabled, [])
findSequenceConditional str@(s:rest) enabled
    | "do()" `isPrefixOf` str = findSequenceConditional rest True
    | "don't()" `isPrefixOf` str = findSequenceConditional rest False
    | not enabled = findSequenceConditional rest enabled
    | otherwise = case str of
        ('m':'u':'l':'(':rest) -> case extractNumber rest ',' of
                (Nothing, ret1) -> findSequenceConditional ret1 enabled
                (Just x, ret1) -> 
                    case extractNumber ret1 ')' of
                        (Nothing, ret2) -> findSequenceConditional ret2 enabled
                        (Just y, ret2) -> (Just (x * y), enabled, ret2)
        _ -> findSequenceConditional rest enabled
