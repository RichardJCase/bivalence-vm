module Parse where

import AST
import Text.Regex.Posix

idRule :: String
idRule = "([a-z][A-Z|a-z]*)|_" --enforce camelCase

typeRule :: String
typeRule = "[A-Z]+" --enforce PascalCase

literalRule :: String
literalRule = "('([^']*)')|[0-9]+"

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

{-
parseLemma :: [String] -> Maybe ExprLemma
parseNative :: [String] -> Maybe ExprNative
parseTypedef :: [String] -> Maybe ExprTypeDef
parseConst :: [String] -> Maybe ExprConst
-}

parseToken :: [String] -> ([String], Maybe Expr)
parseToken tokens = ([], Nothing)
  --rtodo

parseHelper :: [String] -> [Expr] -> Maybe [Expr]
parseHelper [] exprs = Just exprs
parseHelper tokens exprs =
  case parsedExpr of
    Just expr -> parseHelper remainingCode (exprs ++ [expr])
    Nothing -> Nothing
  where
    (remainingCode, parsedExpr) = parseToken tokens

parse :: String -> Maybe [Expr]
parse code = parseHelper (words code) []
