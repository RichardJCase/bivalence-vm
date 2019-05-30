module AST where

data ID = ID String deriving (Show, Eq, Ord)
data Type = Type String deriving Show

data IDList = IDElem ID IDList | EmptyString deriving Show
data ParamList = Param Type ID ParamList | EmptyString deriving Show
