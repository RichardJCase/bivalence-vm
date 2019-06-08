module AST where

data ID = ID String deriving Eq

--todo: any future rules we want here
parseID :: String -> ID
parseID str = ID str

extractID :: ID -> String
extractID (ID str) = str

data Type = Type String deriving Eq

parseType :: String -> Type
parseType str = Type str

extractType :: Type -> String
extractType (Type str) = str

data Operator = Operator String deriving Eq

parseOperator :: String -> Operator
parseOperator str = Operator str

extractOperator :: Operator -> String
extractOperator (Operator op) = op

data Literal = Literal String

parseLiteral :: String -> Literal
parseLiteral str = Literal str

extractLibteral :: Literal -> String
extractLibteral (Literal lit) = lit

data Expr = Expr Signature Operator [Prop] Defn
--todo: add typedefs

data Signature = Signature Type ID [Param]

data Param = Param Type ID

type Defn = Either Application Implication
data OutVars = OutVars Operator [ID]
data Application = Application ID [ID] OutVars

data Implication = Implication ID Operator [ID]
data Prop = Prop ID Operator Defn
