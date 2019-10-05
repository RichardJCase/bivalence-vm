module AST where

data ID = ID String deriving Show
data Type = Type String deriving Show
data Literal = Literal String deriving Show
data ArrowOperator = ArrowOperator String deriving Show
data ColonOperator = ColonOperator String deriving Show
data AtOperator = AtOperator String deriving Show
data OutOperator = OutOperator String deriving Show
data DotOperator = DotOperator String deriving Show
data PoundOperator = PoundOperator String deriving Show

data Expr = Lemma Signature ArrowOperator [Prop] Defn
  | Native AtOperator Signature
  | TypeDef Type ColonOperator Type [Type]
  | Const PoundOperator ID RValue
  deriving Show

data Prop = Prop ID ColonOperator Defn deriving Show
data Signature = Signature ID [Param] deriving Show
data Param = Param Type ID deriving Show

data RValue = RValue (Either ID Literal) deriving Show
type Defn = Either Application Implication
data OutVars = OutVars OutOperator [ID] deriving Show

data Application = Application ID [RValue] (Maybe OutVars) DotOperator deriving Show

data Implication = Implication ID ArrowOperator [ID] ImplicationTail DotOperator deriving Show
type ImplicationTail = [ImplicationTailElem]
data ImplicationTailElem = ImplicationTail ArrowOperator [ID] deriving Show
