import           Control.Monad
import           Data.Char                      ( toUpper )
import           System.IO                      ( IOMode(ReadMode)
                                                , hClose
                                                , hGetContents
                                                , openFile
                                                , withFile
                                                )
main :: IO ()
----------------------------------------
-- with getLine

-- main = forever $ do
--   l <- getLine
--   putStrLn $ map toUpper l

----------------------------------------
-- with getContents

-- main = do
--   contents <- getContents
--   putStrLn $ map toUpper contents

----------------------------------------
-- with shortLinesOnly

-- main = do
--   contents <- getContents
--   putStrLn $ shortLinesOnly contents

-- shortLinesOnly :: String -> String
-- shortLinesOnly = unlines . filter (\l -> length l < 20) . lines

----------------------------------------
-- with interact

-- main = interact shortLinesOnly

-- shortLinesOnly :: String -> String
-- shortLinesOnly = unlines . filter (\l -> length l < 20) . lines

----------------------------------------
-- palindrome

-- main = interact respondPalindromes

-- respondPalindromes :: String -> String
-- respondPalindromes =
--   unlines
--     . map (\l -> if isPal (l) then "palindrome" else "not a palindrome")
--     . lines

-- isPal :: String -> Bool
-- isPal "" = False
-- isPal xs = xs == reverse xs

----------------------------------------
-- with openFile

-- main = do
--   handle   <- openFile "chapter9/haiku.txt" ReadMode
--   contents <- hGetContents handle
--   putStrLn $ map toUpper contents
--   hClose handle

----------------------------------------
-- with withFile

-- main = do
--   withFile "chapter9/haiku.txt" ReadMode $ \handle -> do
--     contents <- hGetContents handle
--     putStrLn $ map toUpper contents

----------------------------------------
-- with readFile

main = do
  contents <- readFile "chapter9/haiku.txt"
  putStrLn $ map toUpper contents
