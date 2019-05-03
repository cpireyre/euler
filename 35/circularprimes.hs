import System.Environment
import Data.List (permutations)

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
nextPrime x | isPrime (x + 1) = x + 1
            | otherwise = nextPrime (x + 1)

digitIsEven :: Char -> Bool
digitIsEven x = (x == '2') || (x == '4') || (x == '6') || (x == '8') || (x == '0') || (x == '5')

noEvenDigits :: Int -> Bool
noEvenDigits x = not $ any digitIsEven (itoa x)

rotateElem :: [a] -> [a]
rotateElem x = last x : init x

allRotates a = take (length a) $ iterate rotateElem a

rotatesArePrime :: Int -> Bool
rotatesArePrime 2 = True
rotatesArePrime 3 = True
rotatesArePrime 5 = True
rotatesArePrime 7 = True
rotatesArePrime x = (noEvenDigits x) && (all isPrime $ map atoi $ allRotates (itoa x))

atoi :: String -> Int
atoi = read

itoa :: Int -> String
itoa = show

main = do
    let answer = filter rotatesArePrime [1..1000000]
    print answer
    print $ length answer
