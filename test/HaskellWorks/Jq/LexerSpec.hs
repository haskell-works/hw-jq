{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Jq.LexerSpec (spec) where

import HaskellWorks.Jq.Ast
import HaskellWorks.Jq.Lexer hiding (operator)
import Test.Hspec
import Text.Parsec

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Jq.LexerSpec" $ do
  it "$.store: The authors of all books" $ do
    parse query "" "$.store" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      ]
  it "$.store.book: The authors of all books" $ do
    parse query "" "$.store.book" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (Field "book")
      ]
  it "$ .store .book: The authors of all books" $ do
    parse query "" "$ .store .book" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (Field "book")
      ]
  it "[*]: The authors of all books" $ do
    parse arrayAccessors "" "[*]" `shouldBe`
      Right (ArrayAccessorOfArraySlice ArraySlice { start = Nothing, stop = Nothing, step = 1 })
  it " [*] : The authors of all books" $ do
    parse arrayAccessors "" " [*] " `shouldBe`
      Right (ArrayAccessorOfArraySlice ArraySlice { start = Nothing, stop = Nothing, step = 1 })
  it " [*] : The authors of all books" $ do
    parse (PathTokenOfArrayAccessor <$> arrayAccessors) "" " [*] " `shouldBe`
      Right (PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice { start = Nothing, stop = Nothing, step = 1 }))
  it "$.store.book[*].author: The authors of all books" $ do
    parse query "" "$.store.book[*].author" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (Field "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice { start = Nothing, stop = Nothing, step = 1 })
      , PathTokenOfFieldAccessor (Field "author")
      ]
  it "$..author: All authors" $ do
    parse query "" "$..author" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "author")
      ]
  it "$.store.*: All things, both books and bicycles" $ do
    parse query "" "$.store.*" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor AnyField
      ]
  it "$.store..price: The price of everything" $ do
    parse query "" "$.store..price" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (Field "store")
      , PathTokenOfFieldAccessor (RecursiveField "price")
      ]
  it "$..book[2]: The third book" $ do
    parse query "" "$..book[2]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArrayRandomAccess (ArrayRandomAccess [2]))
      ]
  it "$..book[0,1]: The first two books" $ do
    parse query "" "$..book[0,1]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArrayRandomAccess (ArrayRandomAccess [0, 1]))
      ]
  it "$..book[:2]: All books from index 0 (inclusive) until index 2 (exclusive)" $ do
    parse query "" "$..book[:2]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Nothing, stop = Just 2, step = 1})
      ]
  it "$..book[1:2]: All books from index 1 (inclusive) until index 2 (exclusive)" $ do
    parse query "" "$..book[1:2]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Just 1, stop = Just 2, step = 1})
      ]
  it "$..book[-2:]: Last two books" $ do
    parse query "" "$..book[-2:]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Just (-2), stop = Nothing, step = 1})
      ]
  it "$..book[-2:]: Last two books" $ do
    parse pathSequence "" "..book[-2:]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice (ArraySlice (Just (-2)) Nothing 1))
      ]
  it "$..book[-2:]: Last two books" $ do
    parse childAccess "" "..book[-2:]" `shouldBe`
      Right (PathTokenOfFieldAccessor (RecursiveField "book"))
  it "$..book[-2:]: Last two books" $ do
    parse arrayAccessors "" "[-2:]" `shouldBe`
      Right (ArrayAccessorOfArraySlice (ArraySlice (Just (-2)) Nothing 1))
  it "$..book[-2:]: Last two books" $ do
    parse arraySlicePartial "" "-2:" `shouldBe`
      Right (ArrayAccessorOfArraySlice (ArraySlice (Just (-2)) Nothing 1))
  it "-1" $ do
    parse pNumber "" "-1" `shouldBe`
      Right (-1)
  it "$..book[2:]: Book number two from tail" $ do
    parse query "" "$..book[2:]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfArrayAccessor (ArrayAccessorOfArraySlice ArraySlice {start = Just 2, stop = Nothing, step = 1})
      ]
  it "$..book[?(@.isbn)]: All books with an ISBN number" $ do
    parse query "" "$..book[?(@.isbn)]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfFilterToken (HasFilter (SubQuery [CurrentNode, PathTokenOfFieldAccessor (Field "isbn")]))
      ]
  it "$.store.book[?(@.price<10)]: All books in store cheaper than 10" $ do
    parse query "" "$.store.book[?(@.price<10)]" `shouldBe`
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
    parse query "" "$..book[?(@.price<=$['expensive'])]" `shouldBe`
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
    parse query "" "$..book[?(@.author=~/.*REES/i)]" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfFilterToken MatchFilter
        { matchValue = SubQuery
          [ CurrentNode
          , PathTokenOfFieldAccessor (Field "author")
          ]
        , matchRegex = RegexLiteral
          { regexString = ".*REES"
          , regexMode = RegexMode
            { regexInsensitive = True
            }
          }
        }
      ]
  it "$..book[?(@.author=~/.*REES/i)]: All books matching regex (ignore case)" $ do
    parse expression "" "@.author=~/.*REES/i" `shouldBe`
      Right (MatchFilter (SubQuery [CurrentNode, PathTokenOfFieldAccessor (Field "author")]) (RegexLiteral ".*REES" (RegexMode True)))
  it "$..book[?(@.author=~/.*REES/i)]: All books matching regex (ignore case)" $ do
    parse expression1 "" "@.author=~/.*REES/i" `shouldBe`
      Right (MatchFilter (SubQuery [CurrentNode, PathTokenOfFieldAccessor (Field "author")]) (RegexLiteral ".*REES" (RegexMode True)))
  it "$..*: Give me every thing" $ do
    parse query "" "$..*" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor RecursiveAnyField
      ]
  it "$..book.length(): The number of books" $ do
    parse query "" "$..book.length()" `shouldBe`
      Right
      [ PathTokenOfFieldAccessor RootNode
      , PathTokenOfFieldAccessor (RecursiveField "book")
      , PathTokenOfFieldAccessor (Field "length")
      ]
