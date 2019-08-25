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
parseParamList :: [String] -> [Param] -> Either ([String], [Param]) [String]
parseParamList [] [] = Right []
parseParamList [] params = Left ([], params)
parseParamList tokens params =
  case parseParam tokens of
    Left (sTokens, sParam) -> parseParamList sTokens (params ++ [sParam])
    Right _ -> if null params then Right tokens else Left (tokens, params)

parseSignature :: [String] -> Either ([String], Signature) [String]
parseSignature (x:[]) =
  case parsedID of
    Just sID -> Left ([], Signature sID [])
    Nothing -> Right [x]
  where
    parsedID = parseID x

parseSignature (x:xs) =
  case parsedID of
    Just sID ->
      case parsedParamList of
        Left (sTokens, sParams) -> Left (sTokens, Signature sID sParams)
        Right _ -> Left (xs, Signature sID [])
    Nothing -> Right (x:xs)
  where
    parsedID = parseID x
    parsedParamList = parseParamList xs []

parseSignature _ = Right []

parseProp :: [String] -> Either ([String], Prop) [String]
--rtodo: obvious refactor
parseProp [] = Right []
parseProp (x:[]) = Right [x]
parseProp (x:xs:[]) = Right (x:xs:[])
parseProp (x:xs:xss) =
  case parseID x of
    Just sID ->
      case parseColonOperator xs of
        Just sColon ->
          case parseDefn xss of
            Left (sTokens, sDefn) -> Left (sTokens, Prop sID sColon sDefn)
            Right _ -> Right xss
        Nothing -> Right (xs:xss)
    Nothing -> Right (x:xs:xss)
  
parsePropList :: [String] -> [Prop] -> Either ([String], [Prop]) [String]
parsePropList [] [] = Right []
parsePropList [] props = Left ([], props)
parsePropList tokens props =
  case parseProp tokens of
    Left (sTokens, sProp) -> parsePropList sTokens (props ++ [sProp])
    Right _ -> if null props then Right tokens else Left (tokens, props)

parseApplication :: [String] -> Either ([String], Application) [String]
parseApplication (idToken:xs) =
  case parseID idToken of
    Just sID ->
      case parseRValueList xs [] of
        Left (rvTokens, sRvalues) ->
          case parseOutVars rvTokens of
            Left (outTokens, sOutvars) -> 
              case parseDotOperator (head rvTokens) of
                Just sDot -> Left (outTokens, Application sID sRvalues (Just sOutvars) sDot)
                Nothing -> Right (idToken:xs)
            Right _ ->
              case parseDotOperator (head rvTokens) of
                Just sDot -> Left (tail rvTokens, Application sID sRvalues Nothing sDot)
                Nothing -> Right rvTokens
        Right _ ->    
          case parseDotOperator (head xs) of
            Just siDot -> Left (tail xs, Application sID [] Nothing siDot)
            Nothing -> Right xs
    Nothing -> Right (idToken:xs)

parseImplication :: [String] -> Either ([String], Implication) [String]
parseImplication (idToken:arrowToken:xs) =
  case parseID idToken of
    Just sID ->
      case parseArrowOperator arrowToken of
        Just sArrow ->
          case parseIDList xs [] of
            Left (sTokens, sIDs) -> do
              let (piTokens, impTail) = parseImplicationTail sTokens []
              case parseDotOperator (head piTokens) of
                Just sDot -> 
                  Left (tail piTokens, Implication sID sArrow sIDs impTail sDot)
                Nothing -> Right piTokens
            Right _ -> Right xs
        Nothing -> Right (arrowToken:xs)
    Nothing -> Right (idToken:arrowToken:xs)

parseImplication _ = Right []

parseImplicationTail :: [String] -> [ImplicationTailElem] -> ([String], ImplicationTail)
parseImplicationTail (arrowToken:xs) implicationTail =
  case parseArrowOperator arrowToken of
    Just sArrow ->
      case parseIDList xs [] of
        Left (sTokens, sIDs) -> parseImplicationTail sTokens (implicationTail ++ [(ImplicationTail sArrow sIDs)])
        Right _ -> (retTokens, implicationTail)
    Nothing -> (retTokens, implicationTail)
  where retTokens = (arrowToken:xs)

--rtodo: replace all right _ with new name and return where error actually was, also clean up
parseDefn :: [String] -> Either ([String], Defn) [String]
parseDefn tokens = 
  case parseApplication tokens of
    Left (appTokens, application) -> Left (appTokens, Left application)
    Right _ ->
      case parseImplication tokens of
        Left (impTokens, implication) -> Left (impTokens, Right implication)
        Right _ -> Right tokens

--rtodo: make function so all of these are not so ugly
parseLemma :: [String] -> Either ([String], Expr) [String]
parseLemma tokens =
  case parseSignature tokens of
    Left ((x:xs), signature) ->
      case parseArrowOperator x of
        Just arrowOperator ->
          case parsePropList xs [] of
            Left (tokens', propList) ->
              case parseDefn tokens' of
                Left (remainingTokens, defn) -> Left (remainingTokens, Lemma signature arrowOperator propList defn)
                Right _ -> Right tokens
            Right _ ->
              case parseDefn xs of
                Left (remainingTokens, defn) -> Left (remainingTokens, Lemma signature arrowOperator [] defn)
                Right _ -> Right tokens
        Nothing -> Right tokens
    Right _ -> Right tokens

parseExprNative :: [String] -> Either ([String], Expr) [String]
parseExprNative (x:xs) =
  case parseAtOperator x of
    Just sAt ->
      case parseSignature xs of
        Left (sTokens, sSignature) -> Left (sTokens, Native sAt sSignature)
        Right _ -> Right xs
    Nothing -> Right (x:xs)

--rtodo: good example, but needs better names
parseExprTypeDef :: [String] -> Either ([String], Expr) [String]
parseExprTypeDef (typeToken:colonToken:aliasToken:xs) =
  case parseType typeToken of
    Nothing -> Right tokens
    Just tID ->
      case parseColonOperator colonToken of
        Nothing -> Right tokens
        Just sColon ->
          case parseType aliasToken of
            Nothing -> Right tokens
            Just aID ->
              case parseTypeList xs [] of
                Left (sTokens, sIDList) -> Left (sTokens, TypeDef tID sColon aID sIDList)
                Right _ -> Left (xs, TypeDef tID sColon aID [])
  where tokens = (typeToken:colonToken:aliasToken:xs)
    
parseExprTypeDef _ = Right []

parseExprConst :: [String] -> Either ([String], Expr) [String]
parseExprConst (poundToken:idToken:valueToken:xs) =
  case poundOperator of
    Just sPound ->
      case id of
        Just sID ->
          case rvalue of
            Just sValue -> Left (xs, Const sPound sID sValue)
            Nothing -> Right tokens
        Nothing -> Right tokens
    Nothing -> Right tokens
  where
    poundOperator = parsePoundOperator poundToken
    id = parseID idToken
    rvalue = parseRValue valueToken
    tokens = (poundToken:idToken:valueToken:xs)

parseExprConst _ = Right []

parseRValue :: String -> Maybe RValue
parseRValue [] = Nothing
parseRValue valueToken =
  case (parseID valueToken) of
        Just rID -> Just $ RValue $ Left rID
        Nothing ->
          case (parseLiteral valueToken) of
            Just rLiteral -> Just $ RValue $ Right rLiteral
            Nothing -> Nothing

parseRValueList :: [String] -> [RValue] -> Either ([String], [RValue]) [String]
parseRValueList (x:xs) rvalues =
  case parseRValue x of
    Just rvalue -> parseRValueList xs (rvalues ++ [rvalue])
    Nothing -> if null rvalues then Right (x:xs) else Left (x:xs, rvalues)

parseRValueList tokens _ = Right tokens

parseOutVars :: [String] -> Either ([String], OutVars) [String]
parseOutVars [] = Right []
parseOutVars (outOpToken:tokens) =
  case parseOutOperator outOpToken of
    Just outOp ->
      case parseIDList tokens [] of
        Left (sTokens, idList) -> Left (sTokens, OutVars outOp idList)
        Right _ -> Right (outOpToken:tokens)
    Nothing -> Right (outOpToken:tokens)

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

