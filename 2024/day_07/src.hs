import Input (input)

numFormed :: [Int] -> Int -> Bool
numFormed arr num
  | length arr == 1 = head arr == num
  | otherwise = numFormed (init arr) (num - last arr) || (num `mod` last arr == 0 && numFormed (init arr) (num `div` last arr))

res :: [(Int, [Int])] -> Int
res [] = 0
res (x : xs)
  | numFormed (snd x) (fst x) = fst x + res xs
  | otherwise = res xs

parseInput :: [String] -> [(Int, [Int])]
parseInput [] = []
parseInput (x : xs) = (head inp, tail inp) : parseInput xs
  where
    strToNum x = read x :: Int
    inp = map strToNum (words x)

main = do
  print . res . parseInput $ input