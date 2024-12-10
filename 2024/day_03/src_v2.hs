import Data.Char (isNumber)
main :: IO()

-- extract product, if correctly formatted string exist
product_if_exist :: String -> Int
product_if_exist str  
    | numExist firstNum  && 
      numExist secondNum && 
      commaExist         && 
      closingBracketExist 
                = strToNum firstNum * strToNum secondNum
    | otherwise = 0
    where
        firstNum            = takeWhile isNumber str
        secondNum           = takeWhile isNumber (drop ((length firstNum)+1) str)
        numExist num        = not (length num == 0)
        commaExist          = str!!(length firstNum) == ','
        closingBracketExist = str!!(length firstNum + length secondNum + 1) == ')'
        strToNum            = (\x -> read x :: Int)

scan :: String -> Bool -> Int
scan str do_enabled
    | length str < 8                                  = 0
    | not do_enabled && startingChars 4 == "do()"     = scanTrue 4
    |     do_enabled && startingChars 7 == "don't()"  = scanFalse 7
    |     do_enabled && startingChars 4 == "mul("     = Main.product_if_exist (drop 4 str) + scanTrue 4
    | otherwise                                       = Main.scan (tail str) do_enabled
    where
        startingChars n = take n str
        scanTrue  n     = Main.scan (drop n str) True
        scanFalse n     = Main.scan (drop n str) False

main = do
    input_content <- readFile "input.txt"
    let res = scan input_content True
    print res
