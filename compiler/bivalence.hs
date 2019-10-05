import System.Environment
import System.Exit
import Data.List
import Data.List.Utils
import Safe
import Parse
import Codegen

usage :: IO ()
usage = do
  putStrLn "Usage: bivalence [options] file..."
  putStrLn "Options:"
  putStrLn "  None yet, lmao"

genOutfileName :: String -> String
genOutfileName filenameBase = filenameBase ++ ".bb"

getOutfileName :: String -> String
getOutfileName filename =
  genOutfileName $ head $ split "." filename

compileFailed :: [String] -> IO ()
compileFailed msg = die $ intercalate "\n" $ msg ++ ["Compilation aborted."]

compile :: String -> IO ()
compile filename = do
  code <- readFile filename
  let output = parse code
  let outfileName = getOutfileName filename

  case output of
    Left ast -> writeFile outfileName $ generateCode ast
    Right messages -> compileFailed messages

main :: IO ()
main = do
  args <- getArgs
  case (nth args 0) of
    Just filename -> do
      compile filename
      putStrLn "You did it fam!"
    Nothing -> usage
