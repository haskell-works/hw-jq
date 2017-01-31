
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Jq.ParserSpec (spec) where

import           HaskellWorks.Jq.Ast
import           HaskellWorks.Jq.Lexer
import           HaskellWorks.Jq.Parser
import           Data.List.Extra (replace)
import           Text.Parsec
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

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
