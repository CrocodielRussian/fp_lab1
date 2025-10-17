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


-- Generate/filter veison

-- Map version

-- Infinity list version


main::IO()

main = print (amicableNumbersSumRec' 1)