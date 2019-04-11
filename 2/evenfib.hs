import System.Environment

sumEvenFibs :: Int -> [Int]
sumEvenFibs x =
    let fib = 1 : 2 : zipWith (+) (fib) (tail fib)
        evens = filter even fib
    in takeWhile (<= x) evens

main = do
        args <- getArgs
        let cutoff = read (args !! 0) -- 4,000,000
            evenfibs = sumEvenFibs (cutoff)
        print (evenfibs)
        print (sum (evenfibs))
