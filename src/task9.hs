checkTripleRec' :: Int -> Int -> Int
checkTripleRec' n m
    | m >= 25 = 1
    | n >= 25 = (checkTripleRec' 0 (m + 1))
    | (2 * m ^ 2 + 2 * m * n == 1000) = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = (checkTripleRec' (n + 1) m)

main = print (checkTripleRec' 0 0)