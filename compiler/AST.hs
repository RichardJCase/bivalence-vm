module AST where

data ID = ID String
data Type = Type String
data Operator = Operator String
data Literal = Literal String

data Expr = ExprLemma | ExprNative | ExprTypeDef | ExprConst

data ExprNative = Native Operator Signature
data ExprTypeDef = TypeDef ID Operator [ID]
data ExprConst = Const ID RValue

data ExprLemma = Lemma Signature Operator [Prop] Defn
data Signature = Signature ID [Param]
data Param = Param Type ID

data RValue = RValue (Either ID Literal)

type Defn = Either Application Implication
data OutVars = OutVars Operator [ID]
data Application = Application ID [RValue] OutVars

data Implication = Implication ID Operator [ID] ImplicationTail
type ImplicationTail = [ImplicationTailElem]
data ImplicationTailElem = ImplicationTail Operator [ID]

data Prop = Prop ID Operator Defn
