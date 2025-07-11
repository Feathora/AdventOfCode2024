import Data.List (elemIndices)
import Combinatorics (variateRep)

main :: IO()
main = do
    input <- readFile "input7.txt"
    let equationLines = lines input

    let equations = [(read x :: Int, map read ops :: [Int], variateRep (length ops - 1) [(*), (+)]) | line <- equationLines, let splitIndex = ':' `indexIn` line, let x = take splitIndex line, let opsLine = drop (splitIndex + 1) line, let ops = words opsLine]

    let total = sum [result | (result, operands, operations) <- equations, let valid = result `elem` ([operation (reverse operands) (reverse ops) | ops <- operations]), valid]
    print total

    let equations' = [(read x :: Int, map read ops :: [Int], variateRep (length ops - 1) [(*), (+), concat']) | line <- equationLines, let splitIndex = ':' `indexIn` line, let x = take splitIndex line, let opsLine = drop (splitIndex + 1) line, let ops = words opsLine]

    let total' = sum [result | (result, operands, operations) <- equations', let valid = result `elem` ([operation (reverse operands) (reverse ops) | ops <- operations]), valid]
    print total'

operation :: [Int] -> [Int -> Int -> Int] -> Int
operation [operand] [] = operand
operation (operand:operands) (operator:operators) = operand `operator` operation operands operators

concat' :: Int -> Int -> Int
concat' l r = read (show r ++ show l) :: Int

indexIn :: Eq a => a -> [a] -> Int
indexIn x xs = head (elemIndices x xs)

removedAt :: Int -> [a] -> [a]
removedAt n xs =
    let (ys, zs) = splitAt n xs
    in ys ++ tail zs