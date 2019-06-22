import System.Environment
import Data.List.Utils
import Safe
import Parse
import Codegen

usage :: IO ()
usage = putStrLn "bad"

genOutfileName :: String -> String
genOutfileName filenameBase = filenameBase ++ ".bb"

getOutfileName :: String -> String
getOutfileName filename =
  genOutfileName $ head $ split "." filename

compileFailed :: IO ()
compileFailed = putStrLn "Compilation aborted."

compile :: String -> IO ()
compile filename =
  case output of
    Just ast -> writeFile outfileName $ generateCode ast
    Nothing -> compileFailed
  where
    output = parse filename
    outfileName = getOutfileName filename

main :: IO ()
main = do
  args <- getArgs
  case (nth args 0) of
    Just filename -> compile filename
    Nothing -> usage
