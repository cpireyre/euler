import System.Environment

sumSquares :: Int -> Int
sumSquares x = sum [i * i | i <- [1 .. x]]

smartSumArith :: Int -> Int
smartSumArith x = (x * (x + 1)) `quot` 2

square :: Int -> Int
square x = x * x

main = do
    let sumSq = sumSquares 100
        squareSm = square (smartSumArith 100)
    print (squareSm - sumSq)
