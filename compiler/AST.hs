module AST where

data ID = ID String
data Type = Type String
data Operator = Operator String
data Literal = Literal String

data ExprList = ExprList Expr ExprList | ExprListEmptyString
data Expr = Expr Signature Operator PropList Defn
data Signature = Signature Type ID ParamList

data IDList = IDList ID IDList | IDListEmptyString
data ParamList = ParamList Type ID ParamList | ParamListEmptyString

data Defn = DefnApplication Application | DefnImplication Implication
data OutVars = OutVars Operator IDList
data Application = Application ID IDList OutVars

data Implication = Implication ID Operator IDList
data Prop = Prop ID Operator Defn
data PropList = PropList Prop PropList | PropListEmptyString
