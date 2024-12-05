import Data.List (sort)

main :: IO()

-- absolute diff
abs :: Int -> Int -> Int
abs a b 
    | a > b = a - b
    | otherwise = b - a

-- length of array
len :: [a] -> Int
len []  = 0
len arr = 1 + len (Main.tail_arr arr)

-- first ele of array
head_ele :: [a] -> a
head_ele arr = arr!!0

-- suffix array of head_ele
tail_arr :: [a] -> [a]
tail_arr (x: xs) = xs

-- subarray with alternate indexes
split :: [a] -> Bool-> [a]
split arr even_idx
    | Main.len arr == 0 = []
    | (Main.len arr `mod` 2 == 0) == even_idx = Main.head_ele arr : split (Main.tail_arr arr) even_idx
    | otherwise = split (Main.tail_arr arr) even_idx

-- distance btw two sorted lists
total_dis :: [Int] -> [Int] -> Int
total_dis l1 l2 =
    if (Main.len l1 == 0 && Main.len l2 == 0)
    then 0
    else Main.abs (Main.head_ele l1) (Main.head_ele l2) +
         Main.total_dis (Main.tail_arr l1) (Main.tail_arr l2)

main = do
    input_content <- readFile "input.txt"
    let l   = map (\x -> read x :: Int) . words $ input_content
        l1  = Main.split l True
        l2  = Main.split l False
        res = total_dis (sort l1) (sort l2)
    print res
