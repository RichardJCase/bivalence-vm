module Codegen where

import AST

generateCode :: [Expr] -> String
generateCode [] = ""
generateCode (token:_) = ""
