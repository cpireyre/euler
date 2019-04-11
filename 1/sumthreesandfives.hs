sumThreesAndFives :: Int -> Int
sumThreesAndFives x =
    let multiples = [i | i <- [1 .. x - 1], i `mod` 3 == 0 || i `mod` 5 == 0]
    in (sum (multiples))

main = print (sumThreesAndFives (1000))
