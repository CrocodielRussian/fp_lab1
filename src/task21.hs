-- Recursion version

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

main::IO()

main = print (amicableNumbersSum' 1 0)