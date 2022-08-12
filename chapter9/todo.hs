import           Control.Exception              ( bracketOnError )
import           Data.List                      ( delete )
import           System.Directory               ( removeFile
                                                , renameFile
                                                )
import           System.Environment             ( getArgs )
import           System.IO                      ( hClose
                                                , hPutStr
                                                , openTempFile
                                                )

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch command  = doesNotExist command


doesNotExist :: String -> [String] -> IO ()
doesNotExist command _ = putStrLn $ "command does not exist: " ++ command

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                  -> putStrLn "command needed."
    (command : argList) -> dispatch command argList


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n"
add _                    = putStrLn "the add command takes exactly 2 arguments."

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let tasks         = lines contents
      numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [0 ..] tasks
  putStrLn "todo tasks are as below:"
  putStrLn $ unlines numberedTasks
view _ = putStrLn "the view command takes exactly 1 argument."

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let tasks       = lines contents
      number      = read numberString
      newTasksStr = unlines $ delete (tasks !! number) tasks
  putStrLn "removing..."
  bracketOnError
    (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName
    )

    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTasksStr
      hClose tempHandle
      removeFile fileName
      renameFile tempName fileName
    )
remove _ = putStrLn "the remove command takes exactly 2 arguments."
