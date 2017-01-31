
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
import           HaskellWorks.Jq.Parser
import           Text.Parsec
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Jq.ParserSpec" $ do
  it "$.store: The authors of all books" $ do
    parse jqFieldName "" "fieldName" `shouldBe`
      Right (JqFieldName "fieldName")
