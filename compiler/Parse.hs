module Parse where

import AST

data ScannerException = ScannerException String deriving Show

--todo: any future rules we want here
parseID :: String -> ID
parseID str = ID str

extractID :: ID -> String
extractID (ID str) = str

parseType :: String -> Type
parseType str = Type str

extractType :: Type -> String
extractType (Type str) = str

parseLiteral :: String -> Literal
parseLiteral str = Literal str

extractLibteral :: Literal -> String
extractLibteral (Literal lit) = lit

parseArrowOperator :: String -> Maybe ArrowOperator
parseArrowOperator "->" = Just $ ArrowOperator "->"
parseArrowOperator _ = Nothing

parseColonOperator :: String -> Maybe ColonOperator
parseColonOperator ":" = Just $ ColonOperator ":"
parseColonOperator _ = Nothing

parseOutOperator :: String -> Maybe OutOperator
parseOutOperator ">" = Just $ OutOperator ">"
parseOutOperator _ = Nothing

parse :: String -> [Expr]
parse code = []
