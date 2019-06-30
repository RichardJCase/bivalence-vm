module Parse where

import AST
import Text.Regex.Posix
import Data.Char

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

parseAtOperator :: String -> Maybe AtOperator
parseAtOperator "@" = Just $ AtOperator "@"
parseAtOperator _ = Nothing

parseColonOperator :: String -> Maybe ColonOperator
parseColonOperator ":" = Just $ ColonOperator ":"
parseColonOperator _ = Nothing

parseOutOperator :: String -> Maybe OutOperator
parseOutOperator ">" = Just $ OutOperator ">"
parseOutOperator _ = Nothing

--rtodo: better variable names
parseParam :: [String] -> Maybe ([String], Param)
parseParam [] = Nothing
parseParam (x:[]) = Nothing
parseParam (x:xs:[]) = Nothing
parseParam (x:xs:xss) =
  case parsedParam of
    Just (sType, sID) -> Just (xss, Param sType sID)
    Nothing -> Nothing
  where
    parsedParam = combine parsedType parsedID
    parsedType = parseType x
    parsedID = parseID xs

--rtodo: simplify these to be recursive in both cases
parseIDList :: [String] -> [ID] -> Maybe ([String], [ID])
parseIDList [] [] = Nothing
parseIDList [] ids = Just ([], ids)
parseIDList tokens ids =
  case parse ID tokens of
    Just (sTokens, sID) -> parseIDList sTokens (ids ++ [sID])
    Nothing -> if null ids then Nothing else Just (tokens, ids)

--rtodo: will need to make function for *_list rules that require at least one
parseParamList :: [String] -> [Param] -> Maybe ([String], [Param])
parseParamList [] [] = Nothing
parseParamList [] params = Just ([], params)
parseParamList tokens params =
  case parseParam tokens of
    Just (sTokens, sParam) -> parseParamList sTokens (params ++ [sParam])
    Nothing -> if null params then Nothing else Just (tokens, params)

parseSignature :: [String] -> Maybe ([String], Signature)
parseSignature [] = Nothing
parseSignature (x:[]) = Nothing
parseSignature (x:xs) =
  case parsedID of
    Just sID ->
      case parsedParamList of
        Just (sTokens, sParams) -> Just (sTokens, Signature sID sParams)
        Nothing -> Nothing
    Nothing -> Nothing
  where
    parsedID = parseID x
    parsedParamList = parseParamList xs []

parseProp :: [String] -> Maybe ([String], Prop)
parseProp [] = Nothing
parseProp (x:[]) = Nothing
parseProp (x:xs:[]) = Nothing
parseProp (x:xs:xss) =
  case parseID x of
    Just sID ->
      case parseColonOperator xs of
        Just sColon ->
          case parseDefn xss of
            Just (sTokens, sDefn) -> Just (sTokens, Prop sID sColon sDefn)
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
  
parsePropList :: [String] -> [Prop] -> Maybe ([String], [Prop])
parsePropList [] [] = Nothing
parsePropList [] props = Just ([], props)
parsePropList tokens props =
  case parseProp tokens of
    Just (sTokens, sProp) -> parsePropList sTokens (props ++ [sProp])
    Nothing -> if null props then Nothing else Just (tokens, props)

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
parseExprNative (x:xs) =
  case parseAtOperator x of
    Just sAt ->
      case parseSignature xs of
        Just (sTokens, sSignature) -> Just (sTokens, Native sAt sSignature)
        Nothing -> Nothing
    Nothing -> Nothing

parseExprTypeDef :: [String] -> Maybe ([String], ExprTypeDef)
parseExprTypeDef (idToken:colonToken:xs) =
  case parseID idToken of
    Just sID ->
      case parseColonOperator colonToken of
        Just sColon ->
          case parseIDList xs of
            Just sIDList -> ExprTypeDef sID sColon sIDList
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing
    
parseExprTypeDef _ = Nothing

parseExprConst :: [String] -> Maybe ([String], ExprConst)
parseExprConst [] = Nothing
parseExprConst tokens = Nothing --rtodo

parseRValue :: [String] -> Maybe ([String], RValue)
parseRValue [] = Nothing
parseRValue tokens = Nothing --rtodo

parseOutVars :: [String] -> Maybe ([String], OutVars)
parseOutVars [] = Nothing
parseOutVars tokens = Nothing --rtodo

--rtodo: make parser generator for this crap
parseExpr :: [String] -> Maybe ([String], Expr)
parseExpr tokens =
  case parseLemma tokens of
    Just (lTokens, sLemma) -> Just (lTokens, sLemma)
    Nothing ->
      case parseExprNative of
        Just (nTokens, sNative) -> Just (nTokens, sNative)
        Nothing ->
          case parseExprTypeDef of
            Just (tTokens, sTypeDef) -> Just (tTokens, sTypeDef)
            Nothing ->
              case parseExprConst of
                Just (cTokens, sConst) -> Just (cTokens, sConst)
                Nothing -> Nothing

parseHelper :: [String] -> [Expr] -> Maybe [Expr]
parseHelper [] exprs = Just exprs
parseHelper tokens exprs =
  case parsedExpr of
    Just (remainingCode, expr) -> parseHelper remainingCode (exprs ++ [expr])
    Nothing -> Nothing
  where
    parsedExpr = parseExpr tokens

--rtodo: error if reach eof
quote :: String -> String -> [String] -> [String]
quote "" "" strings = strings
quote "" currentString strings = strings ++ [currentString]
quote ('\'':xs) currentString strings = tokenize' xs "" (strings ++ [currentString ++ "'"])
quote (x:xs) currentString strings = quote xs (currentString ++ [x]) strings

tokenizeSym :: String -> String -> [String] -> [String]
tokenizeSym "" currentString strings = strings ++ [currentString]
tokenizeSym (x:xs) (cx:cxs) strings =
  case cx of
    '-' -> tokenize' (xs) "" (strings ++ [(cx:x:"")]) --assume ->
    _ ->
      if isNumber cx
      then
        if isNumber $ head cxs
        then tokenizeSym xs ((cx:cxs) ++ [x]) strings
        else tokenize' (x:xs) "" (strings ++ [(cx:cxs)])
      else
        singleChar
    where
      singleChar = tokenize' (x:xs) "" (strings ++ [[cx]])

tokenize' :: String -> String -> [String] -> [String]
tokenize' "" "" strings = strings
tokenize' "" currentString strings = strings ++ [currentString]  
tokenize' (x:xs) currentString strings =
  case x of
    ' ' -> skipChar
    '\t' -> skipChar
    '\n' -> skipChar
    '\'' -> quote xs [x] strings
    _ ->
      if isLetter x then nextChar
      else tokenizeSym xs [x] (strings ++ [currentString])
  where
    skipChar = tokenize' xs "" (strings ++ [currentString])
    nextChar = tokenize' xs (currentString ++ [x]) strings

removeEmpty :: [String] -> [String] -> [String]
removeEmpty [] nonempty = nonempty
removeEmpty (x:xs) nonempty = if x == ""
  then removeEmpty xs nonempty
  else removeEmpty xs (nonempty ++ [x])

tokenize :: String -> [String]
tokenize code = removeEmpty (tokenize' code "" []) []

parse :: String -> Maybe [Expr]
parse code = parseHelper (tokenize code) []

--rtodo: clean up to be like parseExprTypeDef
