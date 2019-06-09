module AST where

data ID = ID String
data Type = Type String
data Literal = Literal String
data ArrowOperator = ArrowOperator String
data ColonOperator = ColonOperator String
data OutOperator = OutOperator String

data Expr = ExprLemma | ExprNative | ExprTypeDef | ExprConst

data ExprNative = Native ArrowOperator Signature
data ExprTypeDef = TypeDef ID ColonOperator [ID]
data ExprConst = Const ID RValue

data Prop = Prop ID ColonOperator Defn
data ExprLemma = Lemma Signature ArrowOperator [Prop] Defn
data Signature = Signature ID [Param]
data Param = Param Type ID

data RValue = RValue (Either ID Literal)

type Defn = Either Application Implication
data OutVars = OutVars OutOperator [ID]
data Application = Application ID [RValue] OutVars

data Implication = Implication ID ArrowOperator [ID] ImplicationTail
type ImplicationTail = [ImplicationTailElem]
data ImplicationTailElem = ImplicationTail ArrowOperator [ID]
