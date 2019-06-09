import AST

--todo: any future rules we want here
parseID :: String -> ID
parseID str = ID str

extractID :: ID -> String
extractID (ID str) = str

parseType :: String -> Type
parseType str = Type str

extractType :: Type -> String
extractType (Type str) = str

parseOperator :: String -> Operator
parseOperator str = Operator str

extractOperator :: Operator -> String
extractOperator (Operator op) = op

parseLiteral :: String -> Literal
parseLiteral str = Literal str

extractLibteral :: Literal -> String
extractLibteral (Literal lit) = lit
