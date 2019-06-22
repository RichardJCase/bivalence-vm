module Parse where

import AST
import Text.Regex.Posix

idRule :: String
idRule = "([a-z][A-Z|a-z]*)|_" --enforce camelCase

typeRule :: String
typeRule = "[A-Z]+[A-Z|a-z]*" --enforce PascalCase

literalRule :: String
literalRule = "('([^']*)')|[0-9]+"

combine :: Maybe a -> Maybe b -> Maybe (a, b)
combine x y =
  case x of
    Just xVal ->
      case y of
        Just yVal -> Just (xVal, yVal)
        Nothing -> Nothing
    Nothing -> Nothing

matchRule :: String -> String -> Maybe String
matchRule text rule =
  if match == text then Just match
  else Nothing
  where match = text =~ rule :: String

parseID :: String -> Maybe ID
parseID str =
  case match of
    Just s -> Just $ ID s
    Nothing -> Nothing
  where
    match = matchRule str idRule

extractID :: ID -> String
extractID (ID str) = str

parseType :: String -> Maybe Type
parseType str = 
  case match of
    Just s -> Just $ Type s
    Nothing -> Nothing
  where
    match = matchRule str typeRule
  
extractType :: Type -> String
extractType (Type str) = str

parseLiteral :: String -> Maybe Literal
parseLiteral str =
  case match of
    Just s -> Just $ Literal s
    Nothing -> Nothing
  where
    match = matchRule str literalRule

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

parseParam :: [String] -> Maybe ([String], Param)
parseParam [] = Nothing
parseParam (x:[]) = Nothing
parseParam (x:xs:xss) =
  case parsedParam of
    Just (sType, sID) -> Just (xss, Param sType sID)
    Nothing -> Nothing
  where
    parsedParam = combine parsedType parsedID
    parsedType = parseType x
    parsedID = parseID xs

--rtodo: will need to make function for *_list rules that require at least one
parseParamList :: [String] -> [Param] -> Maybe ([String], [Param])
parseParamList [] [] = Nothing
parseParamList [] params = Just ([], params)
parseParamList tokens params =
  case parsedParam of
    Just (sTokens, sParam) -> parseParamList sTokens (params ++ [sParam])
    Nothing -> if null params then Nothing else Just (tokens, params)
  where parsedParam = parseParam tokens

parseSignature :: [String] -> Maybe ([String], Signature)
parseSignature [] = Nothing
parseSignature (x:[]) = Nothing
parseSignature (x:xs) =
  case parsedID of
    Just sID ->
      case parsedParamList of
        Just (sTokens, sParams) -> Just $ (sTokens, Signature sID sParams)
        Nothing -> Nothing
    Nothing -> Nothing
  where
    parsedID = parseID x
    parsedParamList = parseParamList xs []

parseProp :: [String] -> Maybe ([String], Prop)
parseProp [] = Nothing
parseProp (x:[]) = Nothing
parseProp (x:xs) = Nothing --rtodo

parsePropList :: [String] -> [Prop] -> Maybe ([String], [Prop])
parsePropList [] [] = Nothing
parsePropList [] props = Just ([], props)
parsePropList tokens props =
  case parsedProp of
    Just (sTokens, sProp) -> parsePropList sTokens (props ++ [sProp])
    Nothing -> if null props then Nothing else Just (tokens, props)
  where parsedProp = parseProp tokens

parseDefn :: [String] -> Maybe ([String], Defn)
parseDefn tokens = Nothing --rtodo

--rtodo: make something not so ugly
parseLemma :: [String] -> Maybe ExprLemma
parseLemma tokens =
  case parseSignature tokens of
    Just ((x:xs), signature) ->
      case parseArrowOperator x of
        Just arrowOperator ->
          case parsePropList xs [] of
            Just (tokens', propList) ->
              case parseDefn tokens' of
                Just (remainingTokens, defn) -> Just $ Lemma signature arrowOperator propList defn
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

parseExprNative :: [String] -> Maybe ([String], ExprNative)
parseExprNative [] = Nothing
parseExprNative tokens = Nothing

parseExprTypeDef :: [String] -> Maybe ([String], ExprTypeDef)
parseExprTypeDef [] = Nothing
parseExprTypeDef tokens = Nothing

parseExprConst :: [String] -> Maybe ([String], ExprConst)
parseExprConst [] = Nothing
parseExprConst tokens = Nothing

parseRValue :: [String] -> Maybe ([String], RValue)
parseRValue [] = Nothing
parseRValue tokens = Nothing

parseOutVars :: [String] -> Maybe ([String], OutVars)
parseOutVars [] = Nothing
parseOutVars tokens = Nothing

parseExpr :: [String] -> Maybe ([String], Expr)
parseExpr tokens = Nothing

parseHelper :: [String] -> [Expr] -> Maybe [Expr]
parseHelper [] exprs = Just exprs
parseHelper tokens exprs =
  case parsedExpr of
    Just (remainingCode, expr) -> parseHelper remainingCode (exprs ++ [expr])
    Nothing -> Nothing
  where
    parsedExpr = parseExpr tokens

tokenize :: String -> [String]
tokenize = words --rtodo: fix for string literals

parse :: String -> Maybe [Expr]
parse code = parseHelper (tokenize code) []
