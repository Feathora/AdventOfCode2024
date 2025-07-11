import Text.Regex.Posix
import Data.List (isPrefixOf)

main :: IO()
main = do
    input <- readFile "input3.txt"
    let pattern = "mul\\(([0-9]+),([0-9]+)\\)"
    let matches = input =~ pattern :: [[String]]

    let result = sum [lhs * rhs | match <- matches, let lhs = read (match !! 1) :: Int, let rhs = read (match !! 2) :: Int]
    print result

    let enablePattern = "(mul)\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"
    let enableMatches = input =~ enablePattern :: [[String]]

    let enabledResult = doMul enableMatches
    print enabledResult

doMul :: [[String]] -> Int
doMul [] = 0
doMul (x:xs)
    | "mul" `isPrefixOf` op = (read (x !! 2) :: Int) * (read (x !! 3) :: Int) + doMul xs
    | "don't" `isPrefixOf` op = don'tMul xs
    | "do" `isPrefixOf` op = doMul xs
    where op = head x

don'tMul :: [[String]] -> Int
don'tMul [] = 0
don'tMul (x:xs)
    | op == "do()" = doMul xs
    | otherwise = don'tMul xs
    where op = head x