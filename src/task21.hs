-- Recursion version

divisionSumRec' :: Int -> Int -> Int
divisionSumRec' n division
    | division > n `div` 2 = 0
    | n `mod` division == 0 = (+) division (divisionSumRec' n (division + 1) )
    | otherwise = divisionSumRec' n (division + 1)

amicableNumbersSumRec' :: Int -> Int
amicableNumbersSumRec' a
    | a > 10000 = 0
    | a /= sum_b = amicableNumbersSumRec' (a+1)
    | a == sum_b = (+) (a + b) (amicableNumbersSumRec' (a+1))
        where b = divisionSumRec' a 1
              sum_b = divisionSumRec' b 1

-- Tail Recurion version

divisionSum' :: Int -> Int -> Int -> Int
divisionSum' n division acc
    | division > n `div` 2 = acc
    | n `mod` division == 0 = divisionSum' n (division + 1) (acc + division)
    | otherwise = divisionSum' n (division + 1) acc

amicableNumbersSum' :: Int -> Int -> Int
amicableNumbersSum' a acc
    | a > 10000 = acc
    | a /= sum_b = amicableNumbersSum' (a+1) acc
    | a == sum_b = amicableNumbersSum' (a+1) (acc + a + b)
        where b = divisionSum' a 1 0
              sum_b = divisionSum' b 1 0 


-- Generate/filter/reduce version

allVariants' = [(x, divisionSum' x 1 0) | x <- [1..10000]]

targetCondition :: (Int, Int) -> Bool
targetCondition (x, y)
    | divisionSum' y 1 0 == x = True
    | otherwise = False

targetCases = filter targetCondition allVariants'

finalSum = foldl (\acc (x, y) -> acc + x + y) 0 targetCases

-- Map version

checkCondition :: (Int, Int) -> Int
checkCondition (x, y)
    | divisionSum' y 1 0 == x = x + y
    | otherwise = 0

mapTargerCases = map checkCondition allVariants'

mapFinalSum = foldl (+) 0 mapTargerCases

-- Infinite list version


divisionSum :: Int -> Int
divisionSum n = sum [d | d <- [1..n `div` 2], n `mod` d == 0]

areAmicable :: Int -> Int -> Bool
areAmicable a b = (a /= b) && (divisionSum a == b) && (divisionSum b == a)

amicableNumbers :: [(Int, Int)]
amicableNumbers = [(a, b) | a <- [1..], let b = divisionSum a, areAmicable a b]

amicableNumbersSum :: Int
amicableNumbersSum =
    sum [a + b | (a, b) <- takeWhile (\(x, _) -> x <= 10000) amicableNumbers]

main::IO()

main = print (amicableNumbersSum)