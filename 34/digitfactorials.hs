fact :: Int -> Int
fact n = product [1..n]

digitFactorial :: Int -> Int
digitFactorial n | n < 10 = fact n
                 | otherwise = fact (n `mod` 10) + (digitFactorial (n `quot` 10))

main = do
    let answer = [x | x <- [145..], digitFactorial x == x]
    print (sum $ take 2 answer)
