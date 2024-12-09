module Main where

main :: IO()
main = do
    contents <- readFile "input.txt"
    let reports = map (map (\word -> read word :: Int) . words) (lines contents)
        safeReports = filter isSafe reports
        safeReportsCount = length safeReports

        dampenedReports = filter canDampen (filter (`notElem` safeReports) reports)
        dampenedReportsCount = safeReportsCount + length dampenedReports

    putStrLn ("Part 1 answer: " ++ show safeReportsCount :: String)
    putStrLn ("Part 2 answer: " ++ show dampenedReportsCount :: String)

-- Part 1 --

areValuesSafe :: Bool -> Int -> Int -> Bool
areValuesSafe asc x y
    -- not ascending nor descending
    | x == y = False
    -- ascending, but we encountered a descending pair
    | asc && x > y = False
    -- descending, but we encountered an ascending pair
    | not asc && y > x = False
    -- otherwise distance must be at least one and at most three
    | otherwise = dist >= 1 && dist <= 3
        where dist = abs (x - y)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe report@(e1:e2:_) = isSafeInternal report (e2 > e1)
    where
        isSafeInternal :: [Int] -> Bool -> Bool
        isSafeInternal [] _ = True
        isSafeInternal [_] _ = True
        isSafeInternal (x:y:rest) asc = areValuesSafe asc x y && isSafeInternal (y:rest) asc

-- Part 2 --

removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

generateLists :: [a] -> [[a]]
generateLists xs = [removeAt i xs | i <- [0..length xs - 1]]

canDampen :: [Int] -> Bool
canDampen report = any isSafe (generateLists report)
