module Codegen where

import Data.List
import AST

generateCode :: [Expr] -> String
generateCode exprs = concat $ map generateExpr exprs

generateExpr :: Expr -> String
generateExpr expr =
  case expr of
    Lemma signature arrowOperator props defn -> generateLemma signature props defn
    Native atOperator signature -> generateNative signature
    TypeDef newName colonOperator derivedType derivedTypes -> generateTypeDef newName (derivedType : derivedTypes)
    Const poundOperator id rvalue -> generateConst id rvalue

generateLemma :: Signature -> [Prop] -> Defn -> String
generateLemma signature props defn = ""

generateSignature :: Signature -> String
generateSignature signature = ""

generateNative :: Signature -> String
generateNative signature = ""

generateTypeDef :: Type -> [Type] -> String
generateTypeDef newType derivedTypes = ""

--todo: check types
generateConst :: ID -> RValue -> String
generateConst (ID id) (RValue (Left (ID value))) = "<const id='" ++ id ++ "'>" ++ value ++ "</const>"
generateConst (ID id) (RValue (Right (Literal value))) = "<const id='" ++ id ++ "'>" ++ value ++ "</const>"  
