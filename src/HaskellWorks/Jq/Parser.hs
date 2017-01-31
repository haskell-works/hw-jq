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

jqFieldNameLead :: Parser u Char
jqFieldNameLead = digit <|> letter <|> underscore

jqFieldNameTail :: Parser u String
jqFieldNameTail = many (digit <|> letter <|> underscore)

jqFieldName :: Parser u JqFieldName
jqFieldName = JqFieldName <$> ((:) <$> jqFieldNameLead <*> jqFieldNameTail)
