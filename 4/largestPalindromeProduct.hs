import System.Environment
import Data.List (sortBy)

safeTail      :: [a] -> [a]
safeTail a
    | (length a == 0) = []
    | otherwise = tail a

safeInit      :: [a] -> [a]
safeInit a
    | (length a == 0) = []
    | otherwise = init a

isPalindrome :: String -> Bool
isPalindrome str
    | (null str)       = True
    | (first == final) = isPalindrome (safeInit $ safeTail str)
    | otherwise        = False
    where   first = head str
            final = last str

tupleProduct  :: (Int, Int) -> Int
tupleProduct (x, y) = x * y

toTriplets    :: (Int, Int) -> (Int, Int, Int)
toTriplets (x, y) = (x, y, tupleProduct (x, y))

getLast       :: (Int, Int, Int) -> Int
getLast (_, _, x) = x

compareLast   :: (Int, Int, Int) -> (Int, Int, Int) -> Ordering
compareLast z k= getLast z `compare` getLast k

getPalindrome :: (Int, Int, Int) -> Bool
getPalindrome z = (isPalindrome (show $ getLast z))


main = do
    let allTuples = [(i,j) | i <- [100..999], j <- [100..999], i < j]
    let triplets = map toTriplets allTuples
    let palindromes = filter getPalindrome triplets
    print (getLast $ head $ reverse $ sortBy compareLast palindromes)
