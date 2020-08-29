
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Jq.ParserSpec (spec) where

import Data.List.Extra        (replace)
import HaskellWorks.Jq.Ast
import HaskellWorks.Jq.Lexer
import HaskellWorks.Jq.Parser
import Test.Hspec
import Text.Parsec

{- HLINT ignore "Redundant do"        -}

escapeDoubleQuote :: String -> String
escapeDoubleQuote = replace "\"" "\\\""

parseOk :: (Eq a, Show a) => Parser () a -> String -> String -> a -> SpecWith (Arg Expectation)
parseOk parser name text expected = it ("parse " ++ name ++ " \"" ++ escapeDoubleQuote text ++ "\"") $ do
  parse parser "" text `shouldBe` Right expected

spec :: Spec
spec = describe "HaskellWorks.Jq.ParserSpec" $ do
  parseOk jqFieldLiteral "jqFieldLiteral" "fieldLiteral" $
    JqFieldName "fieldLiteral"
  parseOk jqSelector     "jqSelector"     ".fieldLiteral" $
    JqSelectorOfFieldLiteral (JqFieldName "fieldLiteral")
  parseOk jqSelector     "jqSelector"     ".[\"fieldLiteral\"]" $
    JqSelectorOfFieldString (JqFieldName "fieldLiteral")
