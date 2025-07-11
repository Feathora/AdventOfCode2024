main :: IO()
main = do
    input <- readFile "input2.txt"
    let inputLines = lines input
    let reports = [[read word :: Int | word <- words line] | line <- inputLines]
    let allSafe = [isSafe report | report <- reports]
    print (length [a | a <- allSafe, a == True])

    let areSafe = [anySafe (permutations report) | report <- reports]
    print (length [a | a <- areSafe, a])

anySafe reportPermutations = or [isSafe report | report <- reportPermutations]

isSafe report = (ascending report || descending report) && bigDiff report

permutations xs = xs : [removedAt n xs | n <- [0..length xs - 1]]

ascending :: [Int] -> Bool
ascending [] = True
ascending (_:[]) = True
ascending (x:xs) = x < head xs && ascending xs

descending :: [Int] -> Bool
descending [] = True
descending (_:[]) = True
descending (x:xs) = x > head xs && descending xs

bigDiff :: [Int] -> Bool
bigDiff [] = True
bigDiff (_:[]) = True
bigDiff (x:xs) =
    let diff = abs (x - head xs)
    in diff >= 1 && diff <= 3 && bigDiff xs

removedAt :: Int -> [a] -> [a]
removedAt n xs =
    let (ys, zs) = splitAt n xs
    in ys ++ tail zs
