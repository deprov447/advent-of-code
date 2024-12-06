main :: IO()

max_level_incr :: [Int] -> Int
max_level_incr (x: xs)
   | xs == [] = 0
   | this_level_incr <= 0 = maxBound :: Int 
   | otherwise = max this_level_incr suffix_max_level_incr
   where
       this_level_incr = head xs - x
       suffix_max_level_incr = max_level_incr xs

isInc :: [Int] -> Bool
isInc (x: xs)
   | xs == [] = True
   | otherwise = x <= head xs && isInc xs

is_safe :: [Int] -> Bool
is_safe og_arr = max_level_incr arr <= 3
    where
        arr | isInc og_arr = og_arr
            | otherwise = reverse og_arr

number_of_safe_reports :: [String] -> Int
number_of_safe_reports [] = 0
number_of_safe_reports (x: xs)
    | is_safe first_report = 1 + suffix_number_of_safe_reports 
    | otherwise = suffix_number_of_safe_reports  
    where
        suffix_number_of_safe_reports = number_of_safe_reports xs
        first_report = map (\y -> read y :: Int) (words x)

main = do
    input_content <- readFile "input.txt"
    let l   = lines input_content
        res = number_of_safe_reports l
    print res
