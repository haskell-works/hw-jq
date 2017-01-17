-- |
-- Copyright: 2017 John Ky
-- License: MIT
--
-- Json

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HaskellWorks.Jq.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as AT
import Data.List.Extra
import Data.Maybe
import Data.Scientific
import HaskellWorks.Jq.Ast

pNumber :: Parser Integer
pNumber = decimal

underscore :: Parser Char
underscore = satisfy (== '_')

hyphen :: Parser Char
hyphen = satisfy (== '-')

field :: Parser String
field = (:)
  <$>      (char '$' <|> digit <|> letter)
  <*> many (char '$' <|> digit <|> letter <|> underscore <|> hyphen)

singleQuotedField :: Parser String
singleQuotedField = replace "\\'" "'" <$> ("'" *> many (satisfy (/= '\'')) <* "'")

doubleQuotedField :: Parser String
doubleQuotedField = replace "\\\"" "\"" <$> ("\"" *> many (satisfy (/= '"')) <* "\"")

singleQuotedValue :: Parser String
singleQuotedValue = replace "\\'" "'" <$> ("'" *> many (satisfy (/= '\'')) <* "'")

doubleQuotedValue :: Parser String
doubleQuotedValue = replace "\\\"" "\"" <$> ("\"" *> many (satisfy (/= '"')) <* "\"")

quotedField :: Parser String
quotedField = singleQuotedField <|> doubleQuotedField

quotedValue :: Parser String
quotedValue = singleQuotedValue <|> doubleQuotedValue

arraySliceStep :: Parser (Maybe Integer)
arraySliceStep = ":" *> optional pNumber

arraySlice :: Parser ArraySlice
arraySlice = sliceOf <$> (":" *> optional pNumber) <*> optional arraySliceStep
  where sliceOf end step' = ArraySlice Nothing end (fromMaybe 1 (join step'))

arrayRandomAccess :: Parser (Maybe ArrayRandomAccess)
arrayRandomAccess = (ArrayRandomAccess <$>) <$> optional (many1 ("," *> pNumber))

arraySlicePartial :: Parser ArrayAccessor
arraySlicePartial = ArrayAccessorOfArraySlice <$> (accessorOf <$> pNumber <*> arraySlice)
  where accessorOf i as = as { start = Just i }

arrayRandomAccessPartial :: Parser ArrayAccessor
arrayRandomAccessPartial = ArrayAccessorOfArrayRandomAccess <$> (accessorOf <$> pNumber <*> arrayRandomAccess)
  where accessorOf i Nothing                            = ArrayRandomAccess [i]
        accessorOf i (Just (ArrayRandomAccess indices)) = ArrayRandomAccess (i : indices)

arrayPartial :: Parser ArrayAccessor
arrayPartial = arraySlicePartial <|> arrayRandomAccessPartial

arrayAll :: Parser ArraySlice
arrayAll = "*" *> return (ArraySlice Nothing Nothing 1)

arrayAccessors :: Parser ArrayAccessor
arrayAccessors = "[" *> arraySpec <* "]"
  where arraySpec
          =   (ArrayAccessorOfArraySlice <$> arrayAll)
          <|> arrayPartial
          <|> (ArrayAccessorOfArraySlice <$> arraySlice)

numberValue :: Parser JPNumber
numberValue = numberOf <$> AT.scientific
  where numberOf :: Scientific -> JPNumber
        numberOf v = case floatingOrInteger v of
          Left r  -> JPDouble r
          Right i -> JPLong i

booleanValue :: Parser FilterDirectValue
booleanValue
  =   "true"  *> return JPTrue
  <|> "false" *> return JPFalse

nullValue :: Parser FilterValue
nullValue = "null" *> return (FilterValueOfFilterDirectValue JPNull)

stringValue :: Parser JPString
stringValue = JPString <$> quotedValue

value :: Parser FilterValue
value
  =   FilterValueOfFilterDirectValue <$> booleanValue
  <|> FilterValueOfFilterDirectValue <$> (FilterDirectValueOfJPNumber <$> numberValue)
  <|> nullValue
  <|> FilterValueOfJPString <$> stringValue

comparisonOperator :: Parser ComparisonOperator
comparisonOperator
  =   ("=="  *> return EqOperator)
  <|> ("!="  *> return NotEqOperator)
  <|> ("<="  *> return LessOrEqOperator)
  <|> ("<"   *> return LessOperator)
  <|> (">="  *> return GreaterOrEqOperator)
  <|> (">"   *> return GreaterOperator)

matchOperator :: Parser MatchOperator
matchOperator = "=~" *> return MatchOperator

current :: Parser PathToken
current = "@" *> return CurrentNode

subQuery :: Parser SubQuery
subQuery = SubQuery <$> ((:) <$> (current <|> root) <*> pathSequence)

expression1 :: Parser FilterToken
expression1 = tokenOf <$> subQuery <*> optional ((,) <$> comparisonOperator <*> ((FilterValueOfSubQuery <$> subQuery) <|> value))
  where tokenOf :: SubQuery -> Maybe (ComparisonOperator, FilterValue) -> FilterToken
        tokenOf subq1 Nothing           = HasFilter subq1
        tokenOf lhs   (Just (op, rhs))  = ComparisonFilter op (FilterValueOfSubQuery lhs) rhs

expression3 :: Parser FilterToken
expression3 = tokenOf <$> value <*> comparisonOperator <*> (FilterValueOfSubQuery <$> subQuery)
  where tokenOf lhs op = ComparisonFilter op lhs

expression :: Parser FilterToken
expression = expression1 <|> expression3 <|> fail "expression"

pBooleanOperator :: Parser BinaryBooleanOperator
pBooleanOperator = ("&&" *> return AndOperator) <|> ("||" *> return OrOperator)

booleanExpression :: Parser FilterToken
booleanExpression = tokenOf <$> expression <*> optional ((,) <$> pBooleanOperator <*> booleanExpression)
  where tokenOf lhs Nothing = lhs
        tokenOf lhs1 (Just (AndOperator, BooleanFilter OrOperator lhs2 rhs2)) =
            BooleanFilter OrOperator (BooleanFilter AndOperator lhs1 lhs2) rhs2
        tokenOf lhs (Just (op, rhs)) = BooleanFilter op lhs rhs

recursiveSubscriptFilter :: Parser RecursiveFilterToken
recursiveSubscriptFilter = RecursiveFilterToken <$> (("..*" <|> "..") *> subscriptFilter)

subscriptFilter :: Parser FilterToken
subscriptFilter = "[?(" *> booleanExpression <* ")]"

subscriptField :: Parser FieldAccessor
subscriptField = subscribe <$> ("[" *> sepBy quotedField "," <* "]")
  where subscribe [f1]    = Field f1
        subscribe fields  = MultiField fields

dotField :: Parser FieldAccessor
dotField = Field <$> ("." *> field)

recursiveField :: Parser FieldAccessor
recursiveField = RecursiveField <$> (".." *> field)

anyChild :: Parser FieldAccessor
anyChild = (".*" <|> "['*']" <|> "[\"*\"]") *> return AnyField

recursiveAny :: Parser FieldAccessor
recursiveAny = "..*" *> return RecursiveAnyField

fieldAccessors :: Parser PathToken
fieldAccessors
  =   (PathTokenOfFieldAccessor         <$> dotField)
  <|> (PathTokenOfRecursiveFilterToken  <$> recursiveSubscriptFilter)
  <|> (PathTokenOfFieldAccessor         <$> recursiveAny)
  <|> (PathTokenOfFieldAccessor         <$> recursiveField)
  <|> (PathTokenOfFieldAccessor         <$> anyChild)
  <|> (PathTokenOfFieldAccessor         <$> subscriptField)

childAccess :: Parser PathToken
childAccess = fieldAccessors <|> (PathTokenOfArrayAccessor <$> arrayAccessors)

pathSequence :: Parser [PathToken]
pathSequence = many (childAccess <|> (PathTokenOfFilterToken <$> subscriptFilter))

root :: Parser PathToken
root = "$" *> return (PathTokenOfFieldAccessor RootNode)

query :: Parser [PathToken]
query = (:) <$> root <*> pathSequence
