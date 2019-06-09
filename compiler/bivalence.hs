import System.Environment
import Safe
import Parse
import Codegen

usage :: IO ()
usage = putStrLn "bad"

main :: IO ()
main = do
  args <- getArgs
  case (nth args 0) of
    Just filename -> writeFile "out.txt" $ generateCode $ parse filename
    Nothing -> usage
