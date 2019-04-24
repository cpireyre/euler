import Data.Array
import Data.List (sortBy)

lencmp x y = length y `compare` length x

nextCollatz :: Int -> Int
nextCollatz n
    | n == 1 = 1
    | even n = n `quot` 2
    | otherwise = 3 * n + 1

tupLatz x = (nextCollatz (fst x), snd x)
goodbye x = fst x /= 1

fullCollatz :: Int -> [Int]
fullCollatz n
    | n == 1 = [1]
    | otherwise = n : fullCollatz (nextCollatz n)

score = length . fullCollatz

compuScores :: [(Int, Int)] -> (Int, Int)
compuScores list
    | length list == 1 = head list
    | otherwise = compuScores $ filter goodbye $ map tupLatz list

main = do
    let burp = [fullCollatz n | n <- [800000..900000]] 
    print $ head $ sortBy lencmp burp
