module Baby where

import           Control.Arrow                  ( Arrow(arr) )
import           Control.Monad                  ( when )
import           Distribution.Simple.Utils      ( xargs )

doubleMe :: Num a => a -> a
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

quickSort :: (Ord a) => [a] -> [a]
quickSort []       = []
quickSort (x : xs) = quickSort smaller ++ [x] ++ quickSort larger
 where
  smaller = [ a | a <- xs, a <= x ]
  larger  = [ a | a <- xs, x < a ]


quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x : xs) =
  let smaller = [ a | a <- xs, a <= x ]
      larger  = [ a | a <- xs, x < a ]
  in  quickSort smaller ++ [x] ++ quickSort larger

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : xy) = f x y : zipWith' f xs xy

chain :: Integer -> [Integer]
chain 1 = [1]
chain x | even x    = x : chain (x `div` 2)
        | otherwise = x : chain (x * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where isLong arr = 15 < length arr

numLongChains' :: Int
numLongChains' =
  length (filter (\arr -> 15 < length arr) (map chain [1 .. 100]))

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []       = a
foldl' f a (x : xs) = foldl f (f a x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f a []       = a
foldr' f a (x : xs) = f x (foldr f a xs)

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' a = foldr (\x acc -> acc && x == a) False

reverse'' :: [a] -> [a]
-- reverse'' = foldr (\x acc -> acc ++ [x]) []  -- slower
reverse'' = foldl (flip (:)) []

last' :: [a] -> a
last' = foldl1 (\acc x -> x)

last'' :: [a] -> a
last'' = foldr1 (\x acc -> acc)

sqrtSums :: Int
sqrtSums =
  length (takeWhile (< 1000) (scanl1 (\acc x -> acc + sqrt x) [1 ..])) + 1


main :: IO ()
main = do
  when True $ do
    putStrLn "hello world."

putStr' :: String -> IO ()
putStr' []       = return ()
putStr' (x : xs) = do
  putChar x
  putStr xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
  putStr' xs
  putChar '\n'
  return ()







