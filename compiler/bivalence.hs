import System.Environment
import Data.List.Utils
import Safe
import Parse
import Codegen

usage :: IO ()
usage = putStrLn "bad"

genOutfileName :: Maybe String -> Maybe String
genOutfileName (Just filenameBase) = Just $ filenameBase ++ ".bb"
genOutfileName Nothing = Nothing

getOutfileName :: String -> Maybe String
getOutfileName filename =
  genOutfileName Nothing (Just filename)

compile :: String -> IO ()
compile filename =
  case outfile of
    Just outfileName -> writeFile outfileName $ generateCode $ parse filename
    Nothing -> putStrLn "bad"
  where outfile = getOutfileName filename

main :: IO ()
main = do
  args <- getArgs
  case (nth args 0) of
    Just filename -> compile filename
    Nothing -> usage
