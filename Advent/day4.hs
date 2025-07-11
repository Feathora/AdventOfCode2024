main :: IO()
main = do
    input <- readFile "input4.txt"
    let inputLines = lines input
    print (findXmas inputLines 0 0)

    print (findMasX inputLines (1, 1))

findXmas :: [String] -> Int -> Int -> Int
findXmas input x y
    | y >= length input = 0
    | x >= length (input !! y) = findXmas input 0 (succ y)
    | (input !! y) !! x == 'X' = (sum [findM input (x, y) dir | dir <- directions]) + findXmas input (succ x) y
    | otherwise = findXmas input (succ x) y
    where directions = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

findM :: [String] -> (Int, Int) -> (Int, Int) -> Int
findM input (x, y) (dx, dy)
    | cy >= length input || cy < 0 = 0
    | cx >= length (input !! cy) || cx < 0 = 0
    | (input !! cy) !! cx == 'M' = findA input (cx, cy) (dx, dy)
    | otherwise = 0
    where cy = y + dy
          cx = x + dx

findA :: [String] -> (Int, Int) -> (Int, Int) -> Int
findA input (x, y) (dx, dy)
    | cy >= length input || cy < 0 = 0
    | cx >= length (input !! cy) || cx < 0 = 0
    | (input !! cy) !! cx == 'A' = findS input (cx, cy) (dx, dy)
    | otherwise = 0
    where cy = y + dy
          cx = x + dx

findS :: [String] -> (Int, Int) -> (Int, Int) -> Int
findS input (x, y) (dx, dy)
    | cy >= length input || cy < 0 = 0
    | cx >= length (input !! cy) || cx < 0 = 0
    | (input !! cy) !! cx == 'S' = 1
    | otherwise = 0
    where cy = y + dy
          cx = x + dx

findMasX :: [String] -> (Int, Int) -> Int
findMasX input (x, y)
    | y >= length input - 1 = 0
    | x >= length (input !! y) - 1 = findMasX input (1, succ y)
    | (input !! y) !! x == 'A' = checkX input (x, y) + findMasX input (succ x, y)
    | otherwise = findMasX input (succ x, y)

checkX :: [String] -> (Int, Int) -> Int
checkX input (x, y)
    | lu == 'M' && rd == 'S' && ru == 'M' && ld == 'S' = 1
    | lu == 'M' && rd == 'S' && ru == 'S' && ld == 'M' = 1
    | lu == 'S' && rd == 'M' && ru == 'M' && ld == 'S' = 1
    | lu == 'S' && rd == 'M' && ru == 'S' && ld == 'M' = 1
    | ru == 'M' && ld == 'S' && lu == 'M' && rd == 'S' = 1
    | ru == 'M' && ld == 'S' && lu == 'S' && rd == 'M' = 1
    | ru == 'S' && ld == 'M' && lu == 'M' && rd == 'S' = 1
    | ru == 'S' && ld == 'M' && lu == 'S' && rd == 'M' = 1
    | otherwise = 0
    where lu = (input !! (y - 1)) !! (x - 1)
          ru = (input !! (y - 1)) !! (x + 1)
          ld = (input !! (y + 1)) !! (x - 1)
          rd = (input !! (y + 1)) !! (x + 1)
