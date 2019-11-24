module Codegen where

import Data.List
import AST

type Symbol = (ID, [Type])
type Generate = (Maybe Symbol, String)

emptyGenerate = (Nothing, "")

generateCode :: [Expr] -> String
generateCode exprs = generateCode' exprs [] ""

generateCode' :: [Expr] -> [Symbol] -> String -> String
generateCode' [] symbols code = code
generateCode' (expr:remainingExprs) symbols code =
  case newSymbols of
    (Just additionalSymbol) -> 
      generateCode' remainingExprs (additionalSymbol:symbols) (code ++ generatedCode)
    Nothing ->
      generateCode' remainingExprs symbols (code ++ generatedCode)
  where (newSymbols, generatedCode) = generateExpr symbols expr

generateExpr :: [Symbol] -> Expr -> Generate
generateExpr symbols expr =
  case expr of
    Lemma signature arrowOperator props defn -> generateLemma symbols signature props defn
    Native atOperator signature -> generateNative signature
    TypeDef newName colonOperator derivedType derivedTypes -> generateTypeDef newName (derivedType : derivedTypes)
    Const poundOperator id rvalue -> generateConst symbols id rvalue

generateProps :: [Symbol] -> [Symbol] -> [Prop] -> String -> Generate
generateProps symbols localSymbols [] code = emptyGenerate
generateProps symbols localSymbols (prop:remainingProps) code =
  case localSymbol of
    Nothing -> emptyGenerate
    Just symbol -> generateProps symbols (symbol:localSymbols) remainingProps newCode
  where (localSymbol, newCode) = generateProp symbols localSymbols prop

generateProp :: [Symbol] -> [Symbol] -> Prop -> Generate
generateProp symbols localSymbols (Prop (ID id) _ defn) =
  case generateDefn (localSymbols ++ symbols) defn of
    "" -> emptyGenerate
    code -> (Nothing, code)

generateDefn :: [Symbol] -> Defn -> String
generateDefn symbols defn =
  case defn of
    Left (Application id rvalues outvars _) -> generateApplication symbols id rvalues outvars
    Right (Implication id _ ids implicationTail _) -> generateImplication symbols id ids implicationTail

generateApplication :: [Symbol] -> ID -> [RValue] -> (Maybe OutVars) -> String
generateApplication symbols (ID id) arguments outvars =
  if definedSym symbols id
  then if mid /= ""
       then start ++ mid ++ end
       else ""          
  else ""
  where start = "<app id=\"" ++ id ++ "\">"
        mid = generateArgs symbols arguments ""
        end = "</app>"

generateArgs :: [Symbol] -> [RValue] -> String -> String
generateArgs symbols [] code = code
generateArgs symbols ((RValue (Left (ID value))):remainingArgs) code =
  if definedSym symbols value
  then generateArgs symbols remainingArgs (code ++ value)
  else ""

generateArgs symbols ((RValue (Right (Literal value))):remainingArgs) code = generateArgs symbols remainingArgs (code ++ value)
  
generateImplication :: [Symbol] -> ID -> [ID] -> ImplicationTail -> String -> String
generateImplication symbols (ID id) resultants implicationTail code =
  if definedSym symbols id
  then do
    case implicationTail of
      [] -> ""
      (tailElem:remainingTail) ->
        concat "<if test=\"\">" ++ (map (\resultant -> generateImplication symbols resultant resultants' code) resultants) ++ "</if>"

generateLemma :: [Symbol] -> Signature -> [Prop] -> Defn -> Generate
generateLemma symbols signature props defn =
  case generateProps (newSym:symbols) [] props "" of
    (Nothing, "") -> emptyGenerate
    (Nothing, code) -> (Just newSym, code)
  where newSym = (sigID, [Type "Lemma"])
        Signature sigID params = signature
        
generateSignature :: Signature -> Generate
generateSignature signature = emptyGenerate

generateNative :: Signature -> Generate
generateNative signature = emptyGenerate

generateTypeDef :: Type -> [Type] -> Generate
generateTypeDef (Type typeName) derivedTypes = ((Just (ID typeName, derivedTypes)), "")

generateConst :: [Symbol] -> ID -> RValue -> Generate
generateConst symbols (ID id) (RValue (Left (ID value))) = emptyGenerate
generateConst symbols (ID id) (RValue (Right (Literal value))) =
  if definedSym symbols id
  then emptyGenerate
  else do
    case idType symbols id of
      Just newID -> (Just (identifier, newID), code)
      Nothing -> emptyGenerate
  where identifier = ID id
        code = "<const id='" ++ id ++ "'>" ++ value ++ "</const>"

idType :: [Symbol] -> String -> Maybe [Type]
idType [] literal = Nothing
idType (sym:rest) literal =
  if (fst sym) == ID literal
  then Just (snd sym)
  else idType rest literal
    
literalType :: String -> Type
literalType literal =
  if isPrefixOf "'" literal
  then Type "String"
  else Type "Nat"

definedSym :: [Symbol] -> String -> Bool
definedSym symbols sym = any (\elm -> (fst elm) == ID sym) symbols
