-- |
-- Copyright: 2017 John Ky
-- License: MIT
--
-- Json

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HaskellWorks.Jq.Types where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
-- import Data.Word (Word8)

data JsonPath

-- jsonpath: dotnotation
--         ;
--
-- dotnotation : '$.' dotnotation_expr ('.' dotnotation_expr)*;
--
-- dotnotation_expr : identifierWithQualifier
--                  | INDENTIFIER
--                  ;
--

data QueryExpr
  = QueryAnd          QueryExpr QueryExpr
  | QueryOr           QueryExpr QueryExpr
  | QueryStar
  | QueryField        String
  | QueryFieldGreater String Int
  | QueryFieldLesser  String Int
  | QueryFieldEqual   String Int
  | QueryFieldNotEqual   String Int
  | QueryLength       Int


data Identifier
  = IdentifierArray
  | IdentifierIndex Int
  | IdentifierQuery QueryExpr

queryExpr :: Parser QueryExpr
queryExpr
  =   QueryAnd            <$> queryExpr <*> ("&&" *> ws *> queryExpr)
  <|> QueryOr             <$> queryExpr <*> ("||" *> ws *> queryExpr)
  <|> const QueryStar     <$> "*"
  <|> QueryFieldGreater   <$> ("@." *> identifier) <*> (">" *> int)
  <|> QueryFieldLesser    <$> ("@." *> identifier) <*> ("<" *> int)
  <|> QueryField          <$> ("@." *> identifier)
  <|> QueryFieldEqual     <$> ("@." *> identifier) <*> ("==" *> int)
  <|> QueryFieldNotEqual  <$> ("@." *> identifier) <*> ("=='" *> int)
  <|> QueryLength         <$> ("@.length-" *> int)

identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlpha <|> satisfy isDigit)

int :: Integral a => Parser a
int = decimal

ws :: Parser ()
ws = satisfy isSpace *> skipWhile isSpace
