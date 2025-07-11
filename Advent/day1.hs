import Data.List (sort)

main :: IO()
main = do
    input <- readFile "input1.txt"
    let inputLines = lines input
    let pairs = parseInput inputLines
    let zipped = uncurry zip pairs
    let diff = sum [abs (uncurry (-) pair) | pair <- zipped]
    print diff

    let similarity = sum [x * (count (snd pairs) x) | x <- fst pairs]
    print similarity

parseInput :: [String] -> ([Int], [Int])
parseInput input = (sort [read (head (words x)) :: Int | x <- input], sort [read (last (words x)) :: Int | x <- input])

count xs find = length [x | x <- xs, x == find]
