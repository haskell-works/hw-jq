module HaskellWorks.Jq.Ast where

data AstToken
  = AstTokenOfPathToken   PathToken
  | AstTokenOfFilterValue FilterValue
  deriving (Eq, Show)

data FieldAccessor
  = RecursiveAnyField
  | AnyField
  | MultiField      [String]
  | RecursiveField  String
  | Field           String
  | RootNode
  deriving (Eq, Show)

data ArrayAccessor
  = ArrayAccessorOfArrayRandomAccess  ArrayRandomAccess
  | ArrayAccessorOfArraySlice         ArraySlice
  deriving (Eq, Show)

newtype ArrayRandomAccess = ArrayRandomAccess [Integer]
  deriving (Eq, Show)

data ArraySlice = ArraySlice
  { start :: Maybe Integer
  , stop  :: Maybe Integer
  , step  :: Integer
  }
  deriving (Eq, Show)

data Any = Null deriving (Eq, Show)

data FilterValue
  = FilterValueOfFilterDirectValue  FilterDirectValue
  | FilterValueOfSubQuery           SubQuery
  | FilterValueOfJPString           JPString
  deriving (Eq, Show)

newtype JPString = JPString String deriving (Eq, Show)

newtype SubQuery = SubQuery [PathToken] deriving (Eq, Show)

data FilterDirectValue
  = JPNull
  | FilterDirectValueOfJPNumber JPNumber
  | JPFalse
  | JPTrue
  deriving (Eq, Show)

data JPNumber
  = JPLong    Integer
  | JPDouble  Double
  deriving (Eq, Show)

data ComparisonOperator
  = EqOperator
  | NotEqOperator
  | LessOperator
  | GreaterOperator
  | LessOrEqOperator
  | GreaterOrEqOperator
  deriving (Eq, Show)

data BinaryBooleanOperator
  = AndOperator
  | OrOperator
  deriving (Eq, Show)

data MatchOperator = MatchOperator deriving (Eq, Show)

newtype RegexMode = RegexMode
  { regexInsensitive :: Bool
  }
  deriving (Eq, Show)

data RegexLiteral = RegexLiteral
  { regexString :: String
  , regexMode   :: RegexMode
  }
  deriving (Eq, Show)

data FilterToken
  = BooleanFilter
    { booleanOperator  :: BinaryBooleanOperator
    , booleanFilterLhs :: FilterToken
    , booleanFilterRhs :: FilterToken
    }
  | ComparisonFilter
    { operator      :: ComparisonOperator
    , comparisonLhs :: FilterValue
    , comparisonRhs :: FilterValue
    }
  | MatchFilter
    { matchValue :: SubQuery
    , matchRegex :: RegexLiteral
    }
  | HasFilter SubQuery
  deriving (Eq, Show)

newtype RecursiveFilterToken = RecursiveFilterToken FilterToken
  deriving (Eq, Show)

data PathToken
  = PathTokenOfFilterToken          FilterToken
  | CurrentNode
  | PathTokenOfArrayAccessor        ArrayAccessor
  | PathTokenOfFieldAccessor        FieldAccessor
  | PathTokenOfRecursiveFilterToken RecursiveFilterToken
  deriving (Eq, Show)

data Relative
  = RelativeCons
    { relativePathToken :: PathToken
    , relativeChild     :: Relative
    }
  | RelativeNil
  deriving (Eq, Show)

newtype Absolute = Absolute Relative deriving (Eq, Show)

data Path = PathOfAbsolute [PathToken] | PathOfRelative [PathToken]

----

newtype JqFieldName = JqFieldName String deriving (Eq, Show)

data JqSelector
  = JqSelectorOfFieldLiteral JqFieldName
  | JqSelectorOfFieldString JqFieldName
  deriving (Eq, Show)

data JqPath = JqPathCons JqSelector | JqPathEmpty deriving (Eq, Show)
