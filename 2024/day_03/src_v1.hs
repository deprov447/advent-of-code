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

scan :: String -> Int
scan str
    | length str < 8       = 0
    | take 4 str == "mul(" = (Main.product_if_exist(drop 4 str)) + Main.scan (drop 4 str)
    | otherwise            = Main.scan (tail str)

main = do
    input_content <- readFile "input.txt"
    let res = scan input_content
    print res
