import Data.List (elemIndices, nub)

main :: IO()
main = do
    input <- readFile "input6.txt"
    let field = lines input

    let startLine = head (filter (elem '^') field)
    let pos = ('^' `index` startLine, startLine `index` field)
    let route = pos : up field pos
    print (length (nub route))

    let variations = obstacles field (0, 0)
    let loops = [up' variation pos (fst pos, snd pos - 1) 0 | variation <- variations]
    print (length (filter id loops))

obstacles :: [String] -> (Int, Int) -> [[String]]
obstacles original (x, y)
    | y == length original = []
    | x == length (original !! y) = obstacles original (0, succ y)
    | (original !! y) !! x == '.' = (take y original ++ [take x row ++ "#" ++ drop (x + 1) row] ++ drop (y + 1) original) : obstacles original (succ x, y)
    | otherwise = obstacles original (succ x, y)
    where row = original !! y

up' :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Bool
up' field startPos pos@(x, y) depth
    | depth > 10000 = True
    | newY < 0 = False
    | pos == startPos = True
    | (field !! newY) !! x == '#' = right' field startPos pos (succ depth)
    | otherwise = up' field startPos newPos (succ depth)
    where newY = y - 1
          newPos = (x, newY)

right' :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Bool
right' field startPos pos@(x, y) depth
    | depth > 10000 = True
    | newX == length (field !! y) = False
    | (field !! y) !! newX == '#' = down' field startPos pos (succ depth)
    | otherwise = right' field startPos newPos (succ depth)
    where newX = x + 1
          newPos = (newX, y)

down' :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Bool
down' field startPos pos@(x, y) depth
    | depth > 10000 = True
    | newY == length field = False
    | (field !! newY) !! x == '#' = left' field startPos pos (succ depth)
    | otherwise = down' field startPos newPos (succ depth)
    where newY = y + 1
          newPos = (x, newY)

left' :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Bool
left' field startPos pos@(x, y) depth
    | depth > 10000 = True
    | newX < 0 = False
    | (field !! y) !! newX == '#' = up' field startPos pos (succ depth)
    | otherwise = left' field startPos newPos (succ depth)
    where newX = x - 1
          newPos = (newX, y)

up :: [String] -> (Int, Int) -> [(Int, Int)]
up field pos@(x, y)
    | newY < 0 = []
    | (field !! newY) !! x == '#' = right field pos
    | otherwise = newPos : up field newPos
    where newY = y - 1
          newPos = (x, newY)

right :: [String] -> (Int, Int) -> [(Int, Int)]
right field pos@(x, y)
    | newX == length (field !! y) = []
    | (field !! y) !! newX == '#' = down field pos
    | otherwise = newPos : right field newPos
    where newX = x + 1
          newPos = (newX, y)

down :: [String] -> (Int, Int) -> [(Int, Int)]
down field pos@(x, y)
    | newY == length field = []
    | (field !! newY) !! x == '#' = left field pos
    | otherwise = newPos : down field newPos
    where newY = y + 1
          newPos = (x, newY)

left :: [String] -> (Int, Int) -> [(Int, Int)]
left field pos@(x, y)
    | newX < 0 = []
    | (field !! y) !! newX == '#' = up field pos
    | otherwise = newPos : left field newPos
    where newX = x - 1
          newPos = (newX, y)

index :: Eq a => a -> [a] -> Int
index x xs = head (elemIndices x xs)