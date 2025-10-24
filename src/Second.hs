module Second where

import Data.List (find)

-- Recursion version

checkTripleRec' :: Int -> Int -> Int
checkTripleRec' n m
    | m >= 25 = 1
    | n >= 25 = checkTripleRec' 0 (m + 1)
    | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = checkTripleRec' (n + 1) m

-- Tail recursion version

allVariantsPairs' :: [(Int, Int)]
allVariantsPairs' = [(x, y) | x <- [1..25], y <- [1..25]]

checkTripleRecTail :: [(Int, Int)] -> Int
checkTripleRecTail [] = 1  
checkTripleRecTail ((m, n) : xs)
  | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
  | otherwise = checkTripleRecTail xs

-- Generate/filter/reduce version

allVariants' = [(n, m) | n <- [1..25], m <- [1..25]]

targetCondition :: (Int, Int) -> Bool
targetCondition (n, m) = 2 * m ^ 2 + 2 * m * n == 1000

targetCase = filter targetCondition allVariants'

targetMul :: [(Int, Int)] -> Int
targetMul = foldl (\acc (n, m) -> acc + (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)) 0

checkTripleReduce = targetMul targetCase

-- Map version

checkCondition :: (Int, Int) -> Int
checkCondition (n, m)
    | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = 0

targetCases' = map checkCondition allVariants'
getCase = filter (> 0) targetCases'

checkTripleMap = head getCase

-- Infinite list version

allVariantsInf' = [(n, m) | n <- [1..], m <- [1..25]]

isValidPair :: (Int, Int) -> Bool
isValidPair (n, m) = 2 * m ^ 2 + 2 * m * n == 1000

computeResult :: Maybe (Int, Int) -> Int
computeResult (Just (n, m)) = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)

checkTripleInfinite :: Maybe (Int, Int)
checkTripleInfinite = find isValidPair allVariantsInf'
