module Scanner where

import Control.Monad
import AST

parse :: String -> Expr
parse fileData =
  --todo

scan' :: String -> IO [Expr]
scan' file = do
  fileData <- readFile file
  return $ parse fileData

scan :: [String] -> IO [Expr]
scan files = concatMapM scan' files

concatMapM :: (a -> IO [Expr]) -> [a] -> IO [Expr]
concatMapM f list = fmap concat $ mapM f list
