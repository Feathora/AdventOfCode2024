import Text.Regex.Posix
import Data.List (elemIndices, permutations)
import Debug.Trace (trace)

rulePattern = "[0-9]+"
pagePattern = "[0-9]+"

main :: IO()
main = do
    input <- readFile "input5.txt"
    let inputLines = lines input

    let rules = [(first, second) | line <- inputLines, '|' `elem` line, let ruleMatches = line =~ rulePattern :: [[String]], let first = read (head (head ruleMatches)) :: Int, let second = read (head (last ruleMatches)) :: Int]
    let updates = [pages | pagesLine <- [line | line <- inputLines, ',' `elem` line], let pageMatches = pagesLine =~ pagePattern :: [[String]], let pages = [read (head page) :: Int | page <- pageMatches]]

    print (sum [processUpdate update rules | update <- updates])

    let invalidUpdates = [update | update <- updates, not (isValid update rules)]
    --let allPermutations = [permutations update | update <- invalidUpdates]
    --let validPermutations = [head valids | permutations <- allPermutations, let valids = [permutation | permutation <- permutations, isValid permutation rules]]
    --print (sum [update !! (length update `div` 2) | update <- validPermutations])
    --print (sum [processUpdate update rules | update <- validPermutations])

    print (sum [fix update rules | update <- invalidUpdates])

fix :: [Int] -> [(Int, Int)] -> Int
fix update rules
    | isValid update rules = update !! (length update `div` 2)
    | otherwise = applyRules update rules 0

applyRules :: [Int] -> [(Int, Int)] -> Int -> Int
applyRules update rules i
    | i >= length rules = fix update rules
    | otherwise = applyRules (applyRule update (rules !! i)) rules (succ i)

applyRule :: [Int] -> (Int, Int) -> [Int]
applyRule update rule@(first, second) = if shouldApply update rule then swapElementsAt (first `index` update) (second `index` update) update else update

shouldApply :: [Int] -> (Int, Int) -> Bool
shouldApply update (first, second)
    | first `notElem` update = False
    | second `notElem` update = False
    | otherwise = first `index` update > second `index` update

isValid :: [Int] -> [(Int, Int)] -> Bool
isValid update rules = and [checkRules update page [(first, second) | (first, second) <- rules, first `elem` update && second `elem` update] | page <- update]

processUpdate :: [Int] -> [(Int, Int)] -> Int
processUpdate update rules = if isValid update rules then update !! (length update `div` 2) else 0

checkRules :: [Int] -> Int -> [(Int, Int)] -> Bool
checkRules pages page rules = and [if page == first then (page `index` pages) < (second `index` pages) else (page `index` pages) > (first `index` pages) | (first, second) <- rules, page == first || page == second]

index :: Eq a => a -> [a] -> Int
index x xs = head (elemIndices x xs)

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs = let smallest = min i j
                            largest = max i j
                            elemI = xs !! smallest
                            elemJ = xs !! largest
                            left = take smallest xs
                            middle = take (largest - smallest - 1) (drop (smallest + 1) xs)
                            right = drop (largest + 1) xs
                    in  left ++ [elemJ] ++ middle ++ [elemI] ++ right
