
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

import           Data.Attoparsec.Text
import           HaskellWorks.Jq.Ast
import           HaskellWorks.Jq.Parser
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Jq.ParserSpec" $ do
  it "$.store.book[*].author: The authors of all books" $ do
    parseOnly query "$.store.book[*].author" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (Field "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice { start = Nothing, stop = Nothing, step = 1 })
      , PathTokenOfFieldAccessor (Field "author")
      ]
  it "$..author: All authors" $ do
    parseOnly query "$..author" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "author")
      ]
  it "$.store.*: All things, both books and bicycles" $ do
    parseOnly query "$.store.*" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor AnyField
      ]
  it "$.store..price: The price of everything" $ do
    parseOnly query "$.store..price" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (RecursiveField "price")
      ]
  it "$..book[2]: The third book" $ do
    parseOnly query "$..book[2]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArrayRandomAccess (ArrayRandomAccess [2]))
      ]
  it "$..book[0,1]: The first two books" $ do
    parseOnly query "$..book[0,1]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArrayRandomAccess (ArrayRandomAccess [0, 1]))
      ]
  it "$..book[:2]: All books from index 0 (inclusive) until index 2 (exclusive)" $ do
    parseOnly query "$..book[:2]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Nothing, stop = Just 2, step = 1})
      ]
  it "$..book[1:2]: All books from index 1 (inclusive) until index 2 (exclusive)" $ do
    parseOnly query "$..book[1:2]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Just 1, stop = Just 2, step = 1})
      ]
  it "$..book[-2:]: Last two books" $ do
    parseOnly query "$..book[-2:]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      ]
  it "$..book[2:]: Book number two from tail" $ do
    parseOnly query "$..book[2:]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Just 2, stop = Nothing, step = 1})
      ]
  it "$..book[?(@.isbn)]: All books with an ISBN number" $ do
    parseOnly query "$..book[?(@.isbn)]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfFilterToken (HasFilter (SubQuery [CurrentNode, PathTokenOfFieldAccessor (Field "isbn")]))
      ]
  it "$.store.book[?(@.price<10)]: All books in store cheaper than 10" $ do
    parseOnly query "$.store.book[?(@.price<10)]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (Field "book")
      , PathTokenOfFilterToken
          ComparisonFilter
          { operator = LessOperator
          , comparisonLhs = FilterValueOfSubQuery
            ( SubQuery
              [ CurrentNode
              , PathTokenOfFieldAccessor (Field "price")
              ]
            )
          , comparisonRhs = FilterValueOfFilterDirectValue (FilterDirectValueOfJPNumber (JPLong 10))
          }
      ]
  it "$..book[?(@.price<=$['expensive'])]: All books in store that are not \"expensive\"" $ do
    parseOnly query "$..book[?(@.price<=$['expensive'])]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfFilterToken
          ComparisonFilter
          { operator = LessOrEqOperator
          , comparisonLhs = FilterValueOfSubQuery
            ( SubQuery
              [ CurrentNode
              , PathTokenOfFieldAccessor (Field "price")
              ]
            )
          , comparisonRhs = FilterValueOfSubQuery
            ( SubQuery
              [ PathTokenOfFieldAccessor RootNode
              , PathTokenOfFieldAccessor (Field "expensive")
              ]
            )
          }
      ]
  it "$..book[?(@.author=~/.*REES/i)]: All books matching regex (ignore case)" $ do
    parseOnly query "$..book[?(@.author=~/.*REES/i)]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      ]
  it "$..*: Give me every thing" $ do
    parseOnly query "$..*" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor RecursiveAnyField
      ]
  it "$..book.length(): The number of books" $ do
    parseOnly query "$..book.length()" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfFieldAccessor (Field "length")
      ]
