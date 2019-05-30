module Scanner where

import Safe
import AST

ss a = putStrLn a
sf = putStrLn "failure"

scan :: IO ()
scan = proceed ss sf $ nth ["success"] 1
