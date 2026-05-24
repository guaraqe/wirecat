{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module WireCat.QQ
  ( effect,
  )
where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isSpace)
import Data.Row.Internal (Empty)
import Data.Row.Records (type (.+), type (.==))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import WireCat.TH (EffectStep, defineEffect, step)

effect :: QuasiQuoter
effect =
  QuasiQuoter
    { quoteDec = parseEffect,
      quoteExp = unsupported "expression",
      quotePat = unsupported "pattern",
      quoteType = unsupported "type"
    }

unsupported :: String -> String -> Q a
unsupported context _ =
  fail $ "effect quasiquoter cannot be used in " ++ context ++ " context"

parseEffect :: String -> Q [Dec]
parseEffect input =
  case filter (not . null) (map (trim . stripLineComment) (lines input)) of
    [] -> fail "empty effect declaration"
    effectName : declarations -> do
      steps <- traverse parseStep declarations
      defineEffect effectName steps

parseStep :: String -> Q EffectStep
parseStep line = do
  (constructorName, rest) <- splitOnce "::" line
  (input, output) <- splitOnce "->" rest
  pure $
    step
      (trim constructorName)
      (parseRowType (trim input))
      (parseRowType (trim output))

parseRowType :: String -> TypeQ
parseRowType "{}" = conT ''Empty
parseRowType row =
  case stripBraces row of
    Nothing -> fail $ "expected row in braces, got: " ++ row
    Just body | null (trim body) -> conT ''Empty
    Just body -> do
      fields <- traverse parseField (splitTopLevel ',' body)
      rowType fields

parseField :: String -> Q (String, Type)
parseField field = do
  (name, ty) <- splitOnce "::" field
  parsedType <- parseSimpleType (trim ty)
  pure (trim name, parsedType)

rowType :: [(String, Type)] -> TypeQ
rowType [] = conT ''Empty
rowType ((name, ty) : fields) = do
  first <- fieldType name ty
  foldM addField first fields
  where
    addField row (fieldName, fieldTy) =
      infixT (pure row) ''(.+) (fieldType fieldName fieldTy)

fieldType :: String -> Type -> TypeQ
fieldType name ty =
  infixT (litT (strTyLit name)) ''(.==) (pure ty)

data Token
  = TokName String
  | TokLParen
  | TokRParen
  | TokLBracket
  | TokRBracket
  | TokComma
  deriving (Eq, Show)

parseSimpleType :: String -> TypeQ
parseSimpleType source = do
  tokens <- tokenize source
  (ty, rest) <- parseType tokens
  case rest of
    [] -> pure ty
    _ -> fail $ "could not parse type suffix in: " ++ source

parseType :: [Token] -> Q (Type, [Token])
parseType tokens = do
  (headType, rest) <- parseAtom tokens
  parseApplications headType rest

parseApplications :: Type -> [Token] -> Q (Type, [Token])
parseApplications ty tokens@(TokName _ : _) = do
  (arg, rest) <- parseAtom tokens
  parseApplications (AppT ty arg) rest
parseApplications ty tokens@(TokLParen : _) = do
  (arg, rest) <- parseAtom tokens
  parseApplications (AppT ty arg) rest
parseApplications ty tokens@(TokLBracket : _) = do
  (arg, rest) <- parseAtom tokens
  parseApplications (AppT ty arg) rest
parseApplications ty tokens =
  pure (ty, tokens)

parseAtom :: [Token] -> Q (Type, [Token])
parseAtom = \case
  TokName name : rest -> do
    resolved <- lookupTypeName name
    case resolved of
      Just typeName -> pure (ConT typeName, rest)
      Nothing -> fail $ "unknown type name: " ++ name
  TokLBracket : rest -> do
    (ty, rest') <- parseType rest
    case rest' of
      TokRBracket : rest'' -> pure (AppT ListT ty, rest'')
      _ -> fail "expected closing ] in list type"
  TokLParen : TokRParen : rest ->
    pure (TupleT 0, rest)
  TokLParen : rest -> do
    (firstType, rest') <- parseType rest
    case rest' of
      TokRParen : rest'' -> pure (firstType, rest'')
      TokComma : rest'' -> parseTuple [firstType] rest''
      _ -> fail "expected closing ) in parenthesized type"
  [] -> fail "expected type"
  token : _ -> fail $ "unexpected token in type: " ++ show token

parseTuple :: [Type] -> [Token] -> Q (Type, [Token])
parseTuple types tokens = do
  (ty, rest) <- parseType tokens
  case rest of
    TokComma : rest' -> parseTuple (types <> [ty]) rest'
    TokRParen : rest' ->
      let tupleTypes = types <> [ty]
       in pure (foldl AppT (TupleT (length tupleTypes)) tupleTypes, rest')
    _ -> fail "expected comma or closing ) in tuple type"

tokenize :: String -> Q [Token]
tokenize [] = pure []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | c == '(' = (TokLParen :) <$> tokenize cs
  | c == ')' = (TokRParen :) <$> tokenize cs
  | c == '[' = (TokLBracket :) <$> tokenize cs
  | c == ']' = (TokRBracket :) <$> tokenize cs
  | c == ',' = (TokComma :) <$> tokenize cs
  | isNameChar c =
      let (name, rest) = span isNameChar (c : cs)
       in (TokName name :) <$> tokenize rest
  | otherwise = fail $ "unexpected character in type: " ++ [c]

isNameChar :: Char -> Bool
isNameChar c =
  isAlphaNum c || c == '_' || c == '\'' || c == '.'

stripBraces :: String -> Maybe String
stripBraces source = do
  stripped <- stripPrefixChar '{' (trim source)
  stripSuffixChar '}' (trim stripped)

stripPrefixChar :: Char -> String -> Maybe String
stripPrefixChar expected source =
  case source of
    c : rest | c == expected -> Just rest
    _ -> Nothing

stripSuffixChar :: Char -> String -> Maybe String
stripSuffixChar expected source =
  case reverse source of
    c : rest | c == expected -> Just (reverse rest)
    _ -> Nothing

splitOnce :: String -> String -> Q (String, String)
splitOnce needle haystack =
  case breakOn needle haystack of
    Nothing -> fail $ "expected " ++ show needle ++ " in: " ++ haystack
    Just parts -> pure parts

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go prefix rest
      | needle `startsWith` rest =
          Just (reverse prefix, drop (length needle) rest)
    go prefix (c : rest) =
      go (c : prefix) rest

startsWith :: String -> String -> Bool
startsWith prefix source =
  take (length prefix) source == prefix

splitTopLevel :: Char -> String -> [String]
splitTopLevel delimiter = go 0 0 [] []
  where
    go :: Int -> Int -> String -> [String] -> String -> [String]
    go _ _ current chunks [] =
      map trim (reverse (reverse current : chunks))
    go parens brackets current chunks (c : cs)
      | c == '(' = go (parens + 1) brackets (c : current) chunks cs
      | c == ')' = go (parens - 1) brackets (c : current) chunks cs
      | c == '[' = go parens (brackets + 1) (c : current) chunks cs
      | c == ']' = go parens (brackets - 1) (c : current) chunks cs
      | c == delimiter && parens == 0 && brackets == 0 =
          go parens brackets [] (reverse current : chunks) cs
      | otherwise = go parens brackets (c : current) chunks cs

stripLineComment :: String -> String
stripLineComment source =
  case breakOn "--" source of
    Nothing -> source
    Just (before, _) -> before

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate =
  reverse . dropWhile predicate . reverse
