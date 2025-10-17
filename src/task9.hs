checkTripleRec' :: Int -> Int -> Int
checkTripleRec' n m
    | m >= 25 = 1
    | n >= 25 = (checkTripleRec' 0 (m + 1))
    | (2 * m ^ 2 + 2 * m * n == 1000) = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = (checkTripleRec' (n + 1) m)


allVariants' = [(n, m) | n <- [1..25], m <- [1..25]]

targetCondition :: (Int, Int) -> Bool
targetCondition (n, m) = 2 * m ^ 2 + 2 * m * n == 1000

targetCase = filter targetCondition allVariants'

targetMul :: [(Int, Int)] -> Int
targetMul [(n, m)] = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)

main = print (targetMul targetCase)

-- main = print (checkTripleRec' 0 0)