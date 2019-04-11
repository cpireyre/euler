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
    | (null divisors == True)    = True
    | otherwise                  = False
    where divisors = listDivisors x

listPrimeFactors :: Int -> [Int] -- 600851475143 in the Euler problem statement.
listPrimeFactors x = filter (isPrime) (listDivisors x)

main = do
    print (maximum $ listPrimeFactors 600851475143) 
