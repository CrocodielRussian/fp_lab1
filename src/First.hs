module First where

-- Recursion version

divisionSumRec' :: Int -> Int -> Int
divisionSumRec' n division
    | division > n `div` 2 = 0
    | n `mod` division == 0 = (+) division (divisionSumRec' n (division + 1) )
    | otherwise = divisionSumRec' n (division + 1)

amicableNumbersSumRec' :: Int -> Int
amicableNumbersSumRec' a
    | a > 10000 = 0
    | a == sum_b && a /= b = (+) (a + b) (amicableNumbersSumRec' (a+1))
    | otherwise = amicableNumbersSumRec' (a+1)
        where b = divisionSumRec' a 1
              sum_b = divisionSumRec' b 1

-- Tail Recurion version

divisionSumRecTail' :: Int -> Int -> Int -> Int
divisionSumRecTail' n division acc
    | division > n `div` 2 = acc
    | n `mod` division == 0 = divisionSumRecTail' n (division + 1) (acc + division)
    | otherwise = divisionSumRecTail' n (division + 1) acc

amicableNumbersSumRecTail' :: Int -> Int -> Int
amicableNumbersSumRecTail' a acc
    | a > 10000 = acc
    | a == sum_b && a /= b = amicableNumbersSumRecTail' (a + 1) (acc + a + b)
    | otherwise = amicableNumbersSumRecTail' (a + 1) acc
    where
      b = divisionSumRecTail' a 1 0
      sum_b = divisionSumRecTail' b 1 0



-- Generate/filter/reduce version

allVariants' = [(x, divisionSumRecTail' x 1 0) | x <- [1..10000]]

targetCondition :: (Int, Int) -> Bool
targetCondition (x, y) = (x /= y) && divisionSumRecTail' y 1 0 == x

targetCasesFilter = filter targetCondition allVariants'

finalSum = foldl (\acc (x, y) -> acc + x + y) 0 targetCasesFilter

-- Map version

checkCondition :: (Int, Int) -> Int
checkCondition (x, y)
    | x /= y && divisionSumRecTail' y 1 0 == x = x + y
    | otherwise = 0

mapTargerCases = map checkCondition allVariants' 

targetCasesMap = sum mapTargerCases

-- Infinite list version

divisionSum :: Int -> Int
divisionSum n = sum [d | d <- [1..(n `div` 2)], n `mod` d == 0]

areAmicable :: Int -> Int -> Bool
areAmicable a b = (a /= b) && (divisionSum a == b) && (divisionSum b == a)

amicableNumbers :: [(Int, Int)]
amicableNumbers = [(a, b) | a <- [1..10000], let b = divisionSum a, areAmicable a b]

amicableNumbersSum :: Int
amicableNumbersSum =
    sum [a + b | (a, b) <- takeWhile (\(x, _) -> x <= 10000) amicableNumbers]