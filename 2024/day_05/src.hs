import Input (rules, updates)

isNotInRules :: [[Int]] -> [Int] -> Bool
isNotInRules [] rule = True
isNotInRules rules rule =
  head rules /= rule && isNotInRules (tail rules) rule

check :: [[Int]] -> Int -> [Int] -> Bool
check rules ele [] = True
check rules ele update =
  isNotInRules rules [head update, ele] && check rules ele (tail update)

isCorrectUpdate :: [[Int]] -> [Int] -> Bool
isCorrectUpdate rules [] = True
isCorrectUpdate rules update =
  check rules (head update) (tail update) && isCorrectUpdate rules (tail update)

filterCorrectUpdates :: [[Int]] -> [[Int]] -> [[Int]]
filterCorrectUpdates rules [] = []
filterCorrectUpdates rules updates
  | isCorrectUpdate rules update = update : filterCorrectUpdates rules (tail updates)
  | otherwise = filterCorrectUpdates rules (tail updates)
  where
    update = head updates

medianSum :: [[Int]] -> Int
medianSum [] = 0
medianSum correctUpdates =
  (mid . head $ correctUpdates) + (medianSum . tail $ correctUpdates)
  where
    mid update = update !! floor ((fromIntegral . length $ update) / 2)

main = do
  let correctUpdates = filterCorrectUpdates rules updates
  print . medianSum $ correctUpdates