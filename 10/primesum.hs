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

-- listPrimes :: Int -> [Int] 
-- listPrimes x = take x $ filter (isPrime) [2..]

-- nextPrime :: Int -> Int
-- nextPrime x = head $ filter (isPrime) [x..]

-- primesUpTo :: Int -> Int -> [Int]
-- primesUpTo start end
--     | end <= start = []
--     | otherwise = next : primesUpTo (next + 1) end
--     where next = nextPrime start

sixK :: Int -> Int -> [Int]
sixK x limit | current >= limit = []
             | otherwise = current - 1 : current + 1 : sixK (x + 1) limit
    where current = 6 * x

main :: IO()
main = do
    let primes = filter isPrime (2 : 3 : sixK 1 2000000)
    print $ sum primes
