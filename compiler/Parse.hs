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

parseDotOperator :: String -> Maybe DotOperator
parseDotOperator "." = Just $ DotOperator "."
parseDotOperator _ = Nothing

parsePoundOperator :: String -> Maybe PoundOperator
parsePoundOperator "#" = Just $ PoundOperator "#"
parsePoundOperator _ = Nothing

--rtodo: better variable names
parseParam :: [String] -> Either ([String], Param) [String]
parseParam (x:[]) = Right [x]
parseParam (x:xs:xss) =
  case parsedParam of
    Just (sType, sID) -> Left (xss, Param sType sID)
    Nothing -> Right rest
  where
    parsedParam = combine parsedType parsedID
    parsedType = parseType x
    parsedID = parseID xs
    rest = (x:xs:xss)

--rtodo: simplify these to be recursive in both cases
parseIDList :: [String] -> [ID] -> Either ([String], [ID]) [String]
parseIDList [] ids = Left ([], ids)
parseIDList tokens ids =
  case parseID (head tokens) of
    Just sID -> parseIDList (tail tokens) (ids ++ [sID])
    Nothing -> if null ids then Right tokens else Left (tokens, ids)

parseTypeList :: [String] -> [Type] -> Either ([String], [Type]) String
parseTypeList [] [] = Right "Failed to parse type list."
parseTypeList [] ids = Left ([], ids)
parseTypeList tokens ids =
  case parseType (head tokens) of
    Just sType -> parseTypeList (tail tokens) (ids ++ [sType])
    Nothing -> if null ids then Right "Failed to parse type list." else Left (tokens, ids)

--rtodo: will need to make function for *_list rules that require at least one
parseParamList :: [String] -> [Param] -> Either ([String], [Param]) String
parseParamList [] [] = Nothing
parseParamList [] params = Just ([], params)
parseParamList tokens params =
  case parseParam tokens of
    Just (sTokens, sParam) -> parseParamList sTokens (params ++ [sParam])
    Nothing -> if null params then Nothing else Just (tokens, params)

parseSignature :: [String] -> Either ([String], Signature) String
parseSignature (x:[]) =
  case parsedID of
    Just sID -> Just ([], Signature sID [])
    Nothing -> Nothing
  where
    parsedID = parseID x

parseSignature (x:xs) =
  case parsedID of
    Just sID ->
      case parsedParamList of
        Just (sTokens, sParams) -> Just (sTokens, Signature sID sParams)
        Nothing -> Just (xs, Signature sID [])
    Nothing -> Nothing
  where
    parsedID = parseID x
    parsedParamList = parseParamList xs []

parseSignature _ = Nothing

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
  
parsePropList :: [String] -> [Prop] -> Either ([String], [Prop]) String
parsePropList [] [] = Nothing
parsePropList [] props = Just ([], props)
parsePropList tokens props =
  case parseProp tokens of
    Just (sTokens, sProp) -> parsePropList sTokens (props ++ [sProp])
    Nothing -> if null props then Nothing else Just (tokens, props)

parseApplication :: [String] -> Maybe ([String], Application)
parseApplication (idToken:xs) =
  case parseID idToken of
    Just sID ->
      case parseRValueList xs [] of
        Just (rvTokens, sRvalues) ->
          case parseOutVars rvTokens of
            Just (outTokens, sOutvars) -> 
              case parseDotOperator (head rvTokens) of
                Just sDot -> Just (outTokens, Application sID sRvalues (Just sOutvars) sDot)
                Nothing -> Nothing
            Nothing ->
              case parseDotOperator (head rvTokens) of
                Just sDot -> Just (tail rvTokens, Application sID sRvalues Nothing sDot)
                Nothing -> Nothing
        Nothing ->
          case parseDotOperator (head xs) of
            Just siDot -> Just (tail xs, Application sID [] Nothing siDot)
            Nothing -> Nothing
    Nothing -> Nothing

parseImplication :: [String] -> Either ([String], Implication) String
parseImplication (idToken:arrowToken:xs) =
  case parseID idToken of
    Just sID ->
      case parseArrowOperator arrowToken of
        Just sArrow ->
          case parseIDList xs [] of
            Just (sTokens, sIDs) -> do
              let (piTokens, impTail) = parseImplicationTail sTokens []
              case parseDotOperator (head piTokens) of
                Just sDot -> 
                  Just (tail piTokens, Implication sID sArrow sIDs impTail sDot)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

parseImplication _ = Nothing

parseImplicationTail :: [String] -> [ImplicationTailElem] -> ([String], ImplicationTail)
parseImplicationTail (arrowToken:xs) implicationTail =
  case parseArrowOperator arrowToken of
    Just sArrow ->
      case parseIDList xs [] of
        Just (sTokens, sIDs) -> parseImplicationTail sTokens (implicationTail ++ [(ImplicationTail sArrow sIDs)])
        Nothing -> (retTokens, implicationTail)
    Nothing -> (retTokens, implicationTail)
  where retTokens = (arrowToken:xs)
    
parseDefn :: [String] -> Maybe ([String], Defn)
parseDefn tokens = 
  case parseApplication tokens of
    Just (appTokens, application) -> Just (appTokens, Left application)
    Nothing ->
      case parseImplication tokens of
        Just (impTokens, implication) -> Just (impTokens, Right implication)
        Nothing -> Nothing

--rtodo: make function so all of these are not so ugly
parseLemma :: [String] -> Either ([String], Expr) [String]
parseLemma tokens =
  case parseSignature tokens of
    Just ((x:xs), signature) ->
      case parseArrowOperator x of
        Just arrowOperator ->
          case parsePropList xs [] of
            Just (tokens', propList) ->
              case parseDefn tokens' of
                Just (remainingTokens, defn) -> Just (remainingTokens, Lemma signature arrowOperator propList defn)
                Nothing -> Nothing
            Nothing ->
              case parseDefn xs of
                Just (remainingTokens, defn) -> Just (remainingTokens, Lemma signature arrowOperator [] defn)
                Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

parseExprNative :: [String] -> Either ([String], Expr) String
parseExprNative (x:xs) =
  case parseAtOperator x of
    Just sAt ->
      case parseSignature xs of
        Just (sTokens, sSignature) -> Just (sTokens, Native sAt sSignature)
        Nothing -> (x:xs)
    Nothing -> (x:xs)

--rtodo: good example, but needs better names
parseExprTypeDef :: [String] -> Either ([String], Expr) String
parseExprTypeDef tokens =
  case parseType typeToken of
    Nothing -> tokens
    Just tID ->
      case parseColonOperator colonToken of
        Nothing -> tokens
        Just sColon ->
          case parseType aliasToken of
            Nothing -> tokens
            Just aID ->
              case parseTypeList xs [] of
                Just (sTokens, sIDList) -> Just (sTokens, TypeDef tID sColon aID sIDList)
                Nothing -> Just (xs, TypeDef tID sColon aID [])
  where tokens = (typeToken:colonToken:aliasToken:xs)
    
parseExprTypeDef _ = Nothing

parseExprConst :: [String] -> Either ([String], Expr) String
parseExprConst (poundToken:idToken:valueToken:xs) =
  case poundOperator of
    Just sPound ->
      case id of
        Just sID ->
          case rvalue of
            Just sValue -> Just (xs, Const sPound sID sValue)
            Nothing -> tokens
        Nothing -> tokens
    Nothing -> tokens
  where
    poundOperator = parsePoundOperator poundToken
    id = parseID idToken
    rvalue = parseRValue valueToken
    tokens = (poundToken:idToken:valueToken:xs)

parseExprConst _ = Nothing

parseRValue :: String -> Maybe RValue
parseRValue [] = Nothing
parseRValue valueToken =
  case (parseID valueToken) of
        Just rID -> Just $ RValue $ Left rID
        Nothing ->
          case (parseLiteral valueToken) of
            Just rLiteral -> Just $ RValue $ Right rLiteral
            Nothing -> Nothing

parseRValueList :: [String] -> [RValue] -> Either ([String], [RValue]) String
parseRValueList (x:xs) rvalues =
  case parseRValue x of
    Just rvalue -> parseRValueList xs (rvalues ++ [rvalue])
    Nothing -> if null rvalues then (x:xs) else Just (x:xs, rvalues)

parseRValueList tokens _ = tokens

parseOutVars :: [String] -> Either ([String], OutVars) String
parseOutVars [] = Nothing
parseOutVars (outOpToken:tokens) =
  case parseOutOperator outOpToken of
    Just outOp ->
      case parseIDList tokens [] of
        Just (sTokens, idList) -> Just (sTokens, OutVars outOp idList)
        Nothing -> (outOpToken:tokens)
    Nothing -> (outOpToken:tokens)

--rtodo: make parser generator for this crap
parseExpr :: [String] -> Either ([String], Expr) [String]
parseExpr tokens =
  case parseLemma tokens of
    Left (lTokens, sLemma) -> Left (lTokens, sLemma)
    Right _ ->
      case parseExprNative tokens of
        Left (nTokens, sNative) -> Left (nTokens, sNative)
        Right _ ->
          case parseExprTypeDef tokens of
            Left (tTokens, sTypeDef) -> Left (tTokens, sTypeDef)
            Right _ ->
              case parseExprConst tokens of
                Left (cTokens, sConst) -> Left (cTokens, sConst)
                Right _ -> Right tokens

parseHelper :: [String] -> [Expr] -> Either [Expr] [String]
parseHelper [] exprs = Left exprs
parseHelper tokens exprs =
  case parsedExpr of
    Left (remainingCode, expr) -> parseHelper remainingCode (exprs ++ [expr])
    Right remaining -> Right remaining 
  where
    parsedExpr = parseExpr tokens

--rtodo: function to generate error message

quote :: String -> String -> [String] -> [String]
quote "" "" strings = strings
quote "" currentString strings = strings ++ [currentString]
quote ('\'':xs) currentString strings = tokenize' xs "" (strings ++ [currentString ++ "'"])
quote (x:xs) currentString strings = quote xs (currentString ++ [x]) strings

--rtodo: refactor
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

parse :: String -> Either [Expr] [String]
parse code = parseHelper (tokenize code) []

