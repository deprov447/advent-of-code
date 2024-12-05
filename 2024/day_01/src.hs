import Data.List (sort)

main :: IO()

-- absolute diff
abs a b 
    | a > b = a - b
    | otherwise = b - a

-- length of array
len []  = 0
len arr = 1 + len (Main.tail_arr arr)

-- first ele of array
head_ele arr = arr!!0

-- suffix array of head_ele
tail_arr (x: xs) = xs

total_dis l1 l2
    | Main.len l1 == 0 && Main.len l2 == 0 = 0
    | otherwise = Main.abs (Main.head_ele l1) (Main.head_ele l2) 
                + Main.total_dis (Main.tail_arr l1) (Main.tail_arr l2)

main = do
    let l1 = [3,4,2,1,3,3]
    let l2 = [4,3,5,3,9,3]
    
    let res = Main.total_dis (sort l1) (sort l2)
    print res
