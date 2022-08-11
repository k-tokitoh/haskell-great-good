import           Control.Arrow                  ( Arrow(arr) )
import           Distribution.Simple.Utils      ( xargs )
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

triangles :: Integer
triangles = 3

lucky :: Int -> String
lucky 7 = "O"
lucky x = "X"

factorical :: Int -> Int
factorical 1 = 1
factorical x = x * factorical (x - 1)

head' :: [a] -> a
head' []      = error "hoge"
head' (h : _) = h

firstLetter :: String -> String
firstLetter ""           = "empty"
firstLetter all@(x : xs) = "first letter of " ++ all ++ " is " ++ [x]

bmi :: Double -> Double -> Double
bmi weight height = weight / height ^ 2

evaluatebmi :: Double -> String
evaluatebmi bmi | bmi <= 18.0 = "thin"
                | bmi <= 25.0 = "normal"
                | otherwise   = "fat"


max' :: (Ord a) => [a] -> a
max' []       = error "hoge"
max' [x     ] = x
max' (x : xs) = max x (max' xs)

replicate' :: Int -> a -> [a]
replicate' n x | n <= 0    = []
               | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x : xs) | n <= 0    = []
                 | otherwise = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _        []       = []
zip' []       _        = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs) | a == x    = True
                 | otherwise = a `elem` xs
