import System.Environment

intSqRoot :: Int -> Int -> Int
intSqRoot x y
    | (x * x >= y)  = x
    | otherwise     = intSqRoot (x + 1) (y)

findSqRt :: Int -> Int
findSqRt x = intSqRoot 0 x

listDivisors :: Int -> [Int]
listDivisors x = [i | i <- [2 .. findSqRt x], x `mod` i == 0]

isPrime :: Int -> Bool
isPrime x
    | (x < 2)                    = False
    | (x == 2)                   = True
    | (x `mod` 2) == 0           = False
    | (null divisors == True)    = True
    | otherwise                  = False
    where divisors = listDivisors x

listPrimes :: Int -> [Int] 
listPrimes x = take x $ filter (isPrime) [2..]

nextPrime :: Int -> Int
nextPrime x = head $ filter (isPrime) [x..]

main = print $ last $ listPrimes 10001
