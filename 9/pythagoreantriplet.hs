isPythagorean :: [Int] -> Bool
isPythagorean x = (a * a + b * b == c * c)
    where a = head x
          b = head $ tail x
          c = last x

main = do
    let triplets = [a:b:c:[] | a <- [1..1000], b <- [1..1000], c <- [1..1000], a + b + c == 1000, a < b, b < c]
    print (product $ head $ filter isPythagorean triplets)
