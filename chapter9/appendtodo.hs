import           Control.Exception              ( bracketOnError )
import           Data.List                      ( delete )
import           System.Directory               ( removeFile
                                                , renameFile
                                                )
import           System.IO                      ( hClose
                                                , hPutStr
                                                , openTempFile
                                                )

main :: IO ()
main = do
  contents <- readFile "chapter9/todo.txt"
  let tasks         = lines contents
      numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [0 ..] tasks
  putStrLn "todo tasks are as below:"
  mapM_ putStrLn numberedTasks

  putStrLn "which one you want to delete?:"
  numberString <- getLine
  let number      = read numberString
      newTasksStr = unlines $ delete (tasks !! number) tasks
  bracketOnError
    (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName
    )

    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTasksStr
      hClose tempHandle
      removeFile "chapter9/todo.txt"
      renameFile tempName "chapter9/todo.txt"
    )

  return ()




