import Data.Set
import Input (field)

newPos :: Int -> Int -> String -> (Int, Int)
newPos sx sy dir
  | dir == "UP" = (sx, sy - 1)
  | dir == "RIGHT" = (sx + 1, sy)
  | dir == "DOWN" = (sx, sy + 1)
  | dir == "LEFT" = (sx - 1, sy)
  | otherwise = (-1002, -1002)

newDir :: String -> String
newDir dir
  | dir == "UP" = "RIGHT"
  | dir == "RIGHT" = "DOWN"
  | dir == "DOWN" = "LEFT"
  | dir == "LEFT" = "UP"
  | otherwise = "PANIC"

move :: [String] -> Int -> Int -> String -> Data.Set.Set (Int, Int) -> Int
move field x y direction set
  | nx < 0 || nx >= length (head field) = 1
  | ny < 0 || ny >= length field = 1
  | member (x, y) set = move field nx ny direction set
  | obstracleAtNext = move field x y (newDir direction) set
  | otherwise = 1 + move field nx ny direction (insert (x, y) set)
  where
    nx = fst (newPos x y direction)
    ny = snd (newPos x y direction)
    obstracleAtNext = (field !! ny) !! nx == '#'

findGuard :: [String] -> (Int, Int)
findGuard field = (x, y)
  where
    y = findy field
    x = findx (field !! y)
    findy field
      | findx (head field) < 99999 = 0
      | otherwise = 1 + findy (tail field)
    findx row
      | length row == 0 = 99999
      | head row == '^' = 0
      | otherwise = 1 + findx (tail row)

noUniqueSquares :: [String] -> Int
noUniqueSquares field = move field x y "UP" Data.Set.empty
  where
    x = fst . findGuard $ field
    y = snd . findGuard $ field

main :: IO ()
main = do
  print . noUniqueSquares $ field