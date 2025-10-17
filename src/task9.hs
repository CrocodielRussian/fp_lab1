-- Recursion version

checkTripleRec' :: Int -> Int -> Int
checkTripleRec' n m
    | m >= 25 = 1
    | n >= 25 = checkTripleRec' 0 (m + 1)
    | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = checkTripleRec' (n + 1) m

-- Generate/filter/reduce version

allVariants' = [(n, m) | n <- [1..25], m <- [1..25]]

targetCondition :: (Int, Int) -> Bool
targetCondition (n, m) = 2 * m ^ 2 + 2 * m * n == 1000

targetCase = filter targetCondition allVariants'

targetMul :: [(Int, Int)] -> Int
targetMul = foldl (\acc (n, m) -> acc + (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)) 0

-- Map version

checkCondition :: (Int, Int) -> Int
checkCondition (n, m)
    | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = 0

getCondition :: Int -> Bool
getCondition x
    | x > 0 = True
    | otherwise = False

targetCases' = map checkCondition allVariants'
getCase = filter getCondition targetCases'

-- Infinite list version

main :: IO ()
main = print (targetMul targetCase)
