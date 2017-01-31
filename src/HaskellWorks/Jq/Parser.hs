-- |
-- Copyright: 2017 John Ky
-- License: MIT
--
-- Json

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HaskellWorks.Jq.Parser where

import Control.Applicative
import HaskellWorks.Jq.Ast
import HaskellWorks.Jq.Lexer
import Text.Parsec.Char
import Text.Parsec.Combinator

jqFieldLiteralLead :: Parser u Char
jqFieldLiteralLead = letter <|> underscore

jqFieldLiteralTail :: Parser u String
jqFieldLiteralTail = many (digit <|> letter <|> underscore)

jqFieldLiteral :: Parser u JqFieldName
jqFieldLiteral = lexeme (JqFieldName <$> ((:) <$> jqFieldLiteralLead <*> jqFieldLiteralTail))

jqSelectorInSubscript :: Parser u JqSelector
jqSelectorInSubscript
  = JqSelectorOfFieldString <$> (JqFieldName <$> stringLiteral)

jqSelector :: Parser u JqSelector
jqSelector = symbol "." *>
  (   JqSelectorOfFieldLiteral <$> jqFieldLiteral
  <|> (symbol "[" *> jqSelectorInSubscript <*  symbol "]")
  )

jqQuery :: Parser u JqSelector
jqQuery = jqSelector <* eof
