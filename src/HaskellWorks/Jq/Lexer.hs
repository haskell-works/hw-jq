{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolymorphicComponents #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -Wno-unused-do-bind       #-}
{-# OPTIONS_GHC -Wno-type-defaults        #-}

module HaskellWorks.Jq.Lexer where

import Control.Monad.Identity
import Data.Char              (digitToInt, isAlpha, isDigit, isSpace, ord, toLower, toUpper)
import Data.Functor
import Data.List              (foldl', nub, sort)
import Data.List.Extra        (replace)
import Data.Maybe
import Data.Scientific        (Scientific)
import HaskellWorks.Jq.Ast
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

import qualified Data.Scientific as Sci

{- HLINT ignore "Use <$>" -}

type Parser u = ParsecT String u Identity

commentStart :: String
commentStart = ""

commentEnd :: String
commentEnd = ""

commentLine :: String
commentLine = ""

nestedComments :: Bool
nestedComments = True

identStart :: Parser u Char
identStart = letter <|> char '_'

identLetter :: Parser u Char
identLetter = alphaNum <|> oneOf "_'"

opStart :: Parser u Char
opStart = opLetter

opLetter :: Parser u Char
opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"

reservedNames :: [String]
reservedNames = []

reservedOpNames :: [String]
reservedOpNames = []

caseSensitive :: Bool
caseSensitive = True

parens :: Parser u a -> Parser u a
parens = between (symbol "(") (symbol ")")

braces :: Parser u a -> Parser u a
braces = between (symbol "{") (symbol "}")

angles :: Parser u a -> Parser u a
angles = between (symbol "<") (symbol ">")

brackets :: Parser u a -> Parser u a
brackets = between (symbol "[") (symbol "]")

semi :: Parser u String
semi = symbol ";"

comma :: Parser u String
comma = symbol ","

dot :: Parser u String
dot = symbol "."

colon :: Parser u String
colon = symbol ":"

commaSep :: Parser u a -> Parser u [a]
commaSep p = sepBy p comma

semiSep :: Parser u a -> Parser u [a]
semiSep p = sepBy p semi

commaSep1 :: Parser u a -> Parser u [a]
commaSep1 p = sepBy1 p comma

semiSep1 :: Parser u a -> Parser u [a]
semiSep1 p = sepBy1 p semi

charLiteral :: Parser u Char
charLiteral = lexeme (between (char '\'')
                                  (char '\'' <?> "end of character")
                                  characterChar )
                <?> "character"

characterChar :: Parser u Char
characterChar = charLetter <|> charEscape
                <?> "literal character"

charEscape :: Parser u Char
charEscape = do { char '\\'; escapeCode }

charLetter :: Parser u Char
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

stringLiteral :: Parser u String
stringLiteral = lexeme (
                  do{ str <- between (char '"')
                                     (char '"' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")

stringChar :: Parser u (Maybe Char)
stringChar =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter :: Parser u Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Parser u (Maybe Char)
stringEscape = do { char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty :: Parser u Char
escapeEmpty = char '&'

escapeGap :: Parser u Char
escapeGap = do
  many1 space
  char '\\' <?> "end of string gap"

escapeCode :: Parser u Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: Parser u Char
charControl = do
  char '^'
  code <- upper
  return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: Parser u Char
charNum = do
  code <- decimal
          <|> (char 'o' >> number 8 octDigit)
          <|> (char 'x' >> number 16 hexDigit)
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

charEsc :: Parser u Char
charEsc = choice (map parseEsc escMap)
  where parseEsc (c, code) = char c >> return code

charAscii :: Parser u Char
charAscii = choice (map parseAscii asciiMap)
  where parseAscii (asc,code) = try (do{ string asc; return code })

escMap :: [(Char, Char)]
escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes = [ "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP" ]

ascii3codes :: [String]
ascii3codes = [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3"
              , "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"]

ascii2 :: String
ascii2 =  [ '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\EM', '\FS', '\GS', '\RS', '\US', '\SP' ]

ascii3 :: String
ascii3 =  [ '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK'
          , '\BEL', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK'
          , '\SYN', '\ETB', '\CAN', '\SUB', '\ESC', '\DEL'
          ]

naturalOrFloat :: Parser u (Either Integer Double)
naturalOrFloat = lexeme natFloat   <?> "number"

float :: Parser u Double
float = lexeme floating   <?> "float"

integer :: Parser u Integer
integer = lexeme int <?> "integer"

natural :: Parser u Integer
natural = lexeme nat <?> "natural"

floating :: Parser u Double
floating = do
  n <- decimal
  fractExponent n

natFloat :: Parser u (Either Integer Double)
natFloat = (char '0' >> zeroNumFloat) <|> decimalFloat

zeroNumFloat :: Parser u (Either Integer Double)
zeroNumFloat = nonDecimal <|> decimalFloat <|> fractFloat 0 <|> return (Left 0)
  where nonDecimal = do
          n <- hexadecimal <|> octal
          return (Left n)

decimalFloat :: Parser u (Either Integer Double)
decimalFloat = do
  n <- decimal
  option (Left n) (fractFloat n)

fractFloat :: (Show a, Read b) => a -> Parser u (Either a1 b)
fractFloat n = do
  f <- fractExponent n
  return (Right f)

fractExponent :: (Show a1, Read a) => a1 -> Parser u a
fractExponent n = fractExponent1 <|> fractExponent2
  where readDouble s = case reads s of
          [(x, "")] -> return x
          _         -> parserZero
        fractExponent1 = do
          fract <- fraction
          expo  <- option "" exponent'
          readDouble (show n ++ fract ++ expo)
        fractExponent2 = do
          expo <- exponent'
          readDouble (show n ++ expo)

fraction ::  Parser u String
fraction = do
      char '.'
      digits <- many1 digit <?> "fraction"
      return ('.' : digits)
  <?> "fraction"

exponent' :: Parser u String
exponent' = do
      oneOf "eE"
      sign' <- fmap (:[]) (oneOf "+-") <|> return ""
      e <- decimal <?> "exponent"
      return ('e' : sign' ++ show e)
  <?> "exponent"

int :: Parser u Integer
int = do
  f <- lexeme sign
  f <$> nat

sign :: Parser u (Integer -> Integer)
sign  =   (char '-' >> return negate)
      <|> (char '+' >> return id)
      <|> return id

nat :: Parser u Integer
nat = zeroNumber <|> decimal

zeroNumber :: Parser u Integer
zeroNumber = do
      char '0'
      hexadecimal <|> octal <|> decimal <|> return 0
  <?> ""

decimal :: Integral a => Parser u a
decimal = number 10 digit

hexadecimal :: Parser u Integer
hexadecimal = oneOf "xX" >> number 16 hexDigit

octal :: Parser u Integer
octal = oneOf "oO" >> number 8 octDigit

number :: (Integral a, Stream s m t) => Integer -> ParsecT s u m Char -> ParsecT s u m a
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return (fromIntegral n))

reservedOp :: String -> Parser u ()
reservedOp name = lexeme $ try $ do
  string name
  notFollowedBy opLetter <?> ("end of " ++ show name)

operator :: Parser u String
operator = lexeme $ try $ do
  name <- oper
  if isReservedOp name
    then unexpected ("reserved operator " ++ show name)
    else return name

oper :: Parser u String
oper = do
      c <- opStart
      cs <- many opLetter
      return (c:cs)
  <?> "operator"

isReservedOp :: String -> Bool
isReservedOp = isReserved (sort reservedOpNames)

reserved :: String -> Parser u ()
reserved name = lexeme $ try $ do
  caseString name
  notFollowedBy identLetter <?> ("end of " ++ show name)

caseString :: Stream s m Char => String -> ParsecT s u m String
caseString name
    | caseSensitive  = string name
    | otherwise      = walk name >> return name
    where
      walk []     = return ()
      walk (c:cs) = do{ caseChar c <?> msg; walk cs }
      caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                  | otherwise  = char c
      msg         = show name

identifier :: Parser u String
identifier = lexeme $ try $ do
  name <- ident
  if isReservedName name
    then unexpected ("reserved word " ++ show name)
    else return name

ident :: Parser u String
ident = do
      c <- identStart
      cs <- many identLetter
      return (c:cs)
  <?> "identifier"

isReservedName :: String -> Bool
isReservedName name = isReserved theReservedNames caseName
  where caseName  | caseSensitive = name
                  | otherwise     = map toLower name

isReserved :: Ord t => [t] -> t -> Bool
isReserved names name = scan names
  where scan []     = False
        scan (r:rs) = case compare r name of
          LT -> scan rs
          EQ -> True
          GT -> False

theReservedNames :: [String]
theReservedNames
    | caseSensitive = sort reserved
    | otherwise     = sort . map (map toLower) $ reserved
    where reserved = reservedNames

symbol :: String -> Parser u String
symbol name = lexeme (string name)

lexeme :: Parser u a -> Parser u a
lexeme p = do
  whiteSpace
  x <- p
  whiteSpace
  return x

whiteSpace :: Parser u ()
whiteSpace
    | noLine && noMulti  = skipMany (simpleSpace <?> "")
    | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
    | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
    | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
    where
      noLine  = null commentLine
      noMulti = null commentStart

simpleSpace :: Parser u ()
simpleSpace =
    skipMany1 (satisfy isSpace)

oneLineComment :: Parser u ()
oneLineComment = do
  try (string commentLine)
  skipMany (satisfy (/= '\n'))
  return ()

multiLineComment :: Parser u ()
multiLineComment = do
  try (string commentStart)
  inComment

inComment :: Parser u ()
inComment
    | nestedComments  = inCommentMulti
    | otherwise       = inCommentSingle

inCommentMulti :: Parser u ()
inCommentMulti
    =   do{ try (string commentEnd) ; return () }
    <|> do{ multiLineComment                     ; inCommentMulti }
    <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
    <|> do{ oneOf startEnd                       ; inCommentMulti }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd ++ commentStart)

inCommentSingle :: Parser u ()
inCommentSingle
    =   do{ try (string commentEnd); return () }
    <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
    <|> do{ oneOf startEnd                      ; inCommentSingle }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd ++ commentStart)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

pNumber :: Parser u Integer
pNumber = mkNumber <$> optionMaybe (symbol "-") <*> decimal
  where mkNumber :: Maybe String -> Integer -> Integer
        mkNumber maybeNegative magnitude = foldl (\x _ -> -x) magnitude maybeNegative

underscore :: Parser u Char
underscore = satisfy (== '_')

hyphen :: Parser u Char
hyphen = satisfy (== '-')

field :: Parser u String
field = (:)
  <$>      (char '$' <|> digit <|> letter)
  <*> many (char '$' <|> digit <|> letter <|> underscore <|> hyphen)

singleQuotedField :: Parser u String
singleQuotedField = replace "\\'" "'" <$> (symbol "'" *> many (satisfy (/= '\'')) <* symbol "'")

doubleQuotedField :: Parser u String
doubleQuotedField = replace "\\\"" "\"" <$> (symbol "\"" *> many (satisfy (/= '"')) <* symbol "\"")

singleQuotedValue :: Parser u String
singleQuotedValue = replace "\\'" "'" <$> (symbol "'" *> many (satisfy (/= '\'')) <* symbol "'")

doubleQuotedValue :: Parser u String
doubleQuotedValue = replace "\\\"" "\"" <$> (symbol "\"" *> many (satisfy (/= '"')) <* symbol "\"")

quotedField :: Parser u String
quotedField = try singleQuotedField <|> doubleQuotedField

quotedValue :: Parser u String
quotedValue = try singleQuotedValue <|> doubleQuotedValue

arraySliceStep :: Parser u (Maybe Integer)
arraySliceStep = symbol ":" *> optionMaybe pNumber

arraySlice :: Parser u ArraySlice
arraySlice = sliceOf <$> (symbol ":" *> optionMaybe pNumber) <*> optionMaybe arraySliceStep
  where sliceOf end step' = ArraySlice Nothing end (fromMaybe 1 (join step'))

arrayRandomAccess :: Parser u (Maybe ArrayRandomAccess)
arrayRandomAccess = (ArrayRandomAccess <$>) <$> optionMaybe (many1 (symbol "," *> pNumber))

arraySlicePartial :: Parser u ArrayAccessor
arraySlicePartial = ArrayAccessorOfArraySlice <$> (accessorOf <$> pNumber <*> arraySlice)
  where accessorOf i as = as { start = Just i }

arrayRandomAccessPartial :: Parser u ArrayAccessor
arrayRandomAccessPartial = ArrayAccessorOfArrayRandomAccess <$> (accessorOf <$> pNumber <*> arrayRandomAccess)
  where accessorOf i Nothing                            = ArrayRandomAccess [i]
        accessorOf i (Just (ArrayRandomAccess indices)) = ArrayRandomAccess (i : indices)

arrayPartial :: Parser u ArrayAccessor
arrayPartial = try arraySlicePartial <|> arrayRandomAccessPartial

arrayAll :: Parser u ArraySlice
arrayAll = symbol "*" $> ArraySlice Nothing Nothing 1

arrayAccessors :: Parser u ArrayAccessor
arrayAccessors = symbol "[" *> arraySpec <* symbol "]"
  where arraySpec
          =   try (ArrayAccessorOfArraySlice <$> arrayAll)
          <|> try arrayPartial
          <|>     (ArrayAccessorOfArraySlice <$> arraySlice)

numberValue :: Parser u JPNumber
numberValue = numberOf <$> scientific
  where numberOf :: Scientific -> JPNumber
        numberOf v = case Sci.floatingOrInteger v of
          Left r  -> JPDouble r
          Right i -> JPLong i

booleanValue :: Parser u FilterDirectValue
booleanValue
  =   try (symbol "true" $> JPTrue)
  <|>     symbol "false" $> JPFalse

nullValue :: Parser u FilterValue
nullValue = symbol "null" $> FilterValueOfFilterDirectValue JPNull

stringValue :: Parser u JPString
stringValue = JPString <$> quotedValue

value :: Parser u FilterValue
value
  =   try (FilterValueOfFilterDirectValue <$> booleanValue)
  <|> try (FilterValueOfFilterDirectValue <$> (FilterDirectValueOfJPNumber <$> numberValue))
  <|> try nullValue
  <|>     FilterValueOfJPString <$> stringValue

comparisonOperator :: Parser u ComparisonOperator
comparisonOperator
  =   try (symbol "=="  $> EqOperator)
  <|> try (symbol "!="  $> NotEqOperator)
  <|> try (symbol "<="  $> LessOrEqOperator)
  <|> try (symbol "<"   $> LessOperator)
  <|> try (symbol ">="  $> GreaterOrEqOperator)
  <|>     (symbol ">"   $> GreaterOperator)

matchOperator :: Parser u MatchOperator
matchOperator = symbol "=~" $> MatchOperator

current :: Parser u PathToken
current = symbol "@" $> CurrentNode

subQuery :: Parser u SubQuery
subQuery = SubQuery <$> ((:) <$> (try current <|> root) <*> pathSequence)

expression2 :: Parser u FilterToken
expression2 = tokenOf <$> subQuery <*> optionMaybe ((,) <$> comparisonOperator <*> (try (FilterValueOfSubQuery <$> subQuery) <|> value))
  where tokenOf :: SubQuery -> Maybe (ComparisonOperator, FilterValue) -> FilterToken
        tokenOf subq1 Nothing          = HasFilter subq1
        tokenOf lhs   (Just (op, rhs)) = ComparisonFilter op (FilterValueOfSubQuery lhs) rhs

pRegexMode :: Parser u RegexMode
pRegexMode = RegexMode <$> (isJust <$> optionMaybe (char 'i'))

regexLiteral :: Parser u RegexLiteral
regexLiteral = RegexLiteral <$> (char '/' *> many1 (satisfy (/= '/')) <* char '/') <*> pRegexMode

expression1 :: Parser u FilterToken
expression1 = tokenOf <$> subQuery <*> matchOperator <*> regexLiteral
  where tokenOf :: SubQuery -> MatchOperator -> RegexLiteral -> FilterToken
        tokenOf subQuery _ = MatchFilter subQuery

expression3 :: Parser u FilterToken
expression3 = tokenOf <$> value <*> comparisonOperator <*> (FilterValueOfSubQuery <$> subQuery)
  where tokenOf lhs op = ComparisonFilter op lhs

expression :: Parser u FilterToken
expression = try expression1 <|> try expression2 <|> try expression3 <|> fail "expression"

pBooleanOperator :: Parser u BinaryBooleanOperator
pBooleanOperator = try (symbol "&&" $> AndOperator) <|> (symbol "||" $> OrOperator)

booleanExpression :: Parser u FilterToken
booleanExpression = tokenOf <$> expression <*> optionMaybe ((,) <$> pBooleanOperator <*> booleanExpression)
  where tokenOf lhs Nothing = lhs
        tokenOf lhs1 (Just (AndOperator, BooleanFilter OrOperator lhs2 rhs2)) =
            BooleanFilter OrOperator (BooleanFilter AndOperator lhs1 lhs2) rhs2
        tokenOf lhs (Just (op, rhs)) = BooleanFilter op lhs rhs

recursiveSubscriptFilter :: Parser u RecursiveFilterToken
recursiveSubscriptFilter = RecursiveFilterToken <$> ((try (symbol "..*") <|> symbol "..") *> subscriptFilter)

subscriptFilter :: Parser u FilterToken
subscriptFilter = symbol "[?(" *> booleanExpression <* symbol ")]"

subscriptField :: Parser u FieldAccessor
subscriptField = subscribe <$> (symbol "[" *> sepBy quotedField (symbol ",") <* symbol "]")
  where subscribe [f1]   = Field f1
        subscribe fields = MultiField fields

dotField :: Parser u FieldAccessor
dotField = Field <$> (symbol "." *> field)

recursiveField :: Parser u FieldAccessor
recursiveField = RecursiveField <$> (symbol ".." *> field)

anyChild :: Parser u FieldAccessor
anyChild = (try (symbol ".*") <|> try (symbol "['*']") <|> symbol "[\"*\"]") $> AnyField

recursiveAny :: Parser u FieldAccessor
recursiveAny = symbol "..*" $> RecursiveAnyField

fieldAccessors :: Parser u PathToken
fieldAccessors
  =   try (PathTokenOfFieldAccessor         <$> dotField)
  <|> try (PathTokenOfRecursiveFilterToken  <$> recursiveSubscriptFilter)
  <|> try (PathTokenOfFieldAccessor         <$> recursiveAny)
  <|> try (PathTokenOfFieldAccessor         <$> recursiveField)
  <|> try (PathTokenOfFieldAccessor         <$> anyChild)
  <|>     (PathTokenOfFieldAccessor         <$> subscriptField)

childAccess :: Parser u PathToken
childAccess = try fieldAccessors <|> (PathTokenOfArrayAccessor <$> arrayAccessors)

pathStep :: Parser u PathToken
pathStep = try childAccess <|> (PathTokenOfFilterToken <$> subscriptFilter)

pathSequence :: Parser u [PathToken]
pathSequence = many pathStep

root :: Parser u PathToken
root = symbol "$" $> PathTokenOfFieldAccessor RootNode

query :: Parser u [PathToken]
query = (:) <$> root <*> pathSequence

path :: Parser u Path
path
  =   PathOfAbsolute <$> try (root *> pathSequence)
  <|> PathOfRelative <$> pathSequence

-----

scientific :: Parser u Scientific
scientific = scientifically id

-- A strict pair
data SP = SP !Integer {-# UNPACK #-}!Int

takeWhile' :: (Char -> Bool) -> Parser u String
takeWhile' p = (:) <$> satisfy p <*> takeWhile' p <|> return ""

{-# INLINE scientifically #-}
scientifically :: (Scientific -> a) -> Parser u a
scientifically h = do
  !positive <- ((== '+') <$> satisfy (\c -> c == '-' || c == '+')) <|>
               pure True

  n <- decimal

  let f fracDigits = SP (foldl' step n fracDigits)
                        (negate $ length fracDigits)
      step a c = a * 10 + fromIntegral (ord c - 48)

  SP c e <- (satisfy (=='.') *> (f <$> takeWhile' isDigit)) <|>
            pure (SP n 0)

  let !signedCoeff | positive  =  c
                   | otherwise = -c

  (satisfy (\w -> w == 'e' || w == 'E') *>
      fmap (h . Sci.scientific signedCoeff . (e +)) (signed decimal)) <|>
    return (h $ Sci.scientific signedCoeff    e)

signed :: Num a => Parser u a -> Parser u a
signed p = (negate <$> (char '-' *> p))
       <|> (char '+' *> p)
       <|> p
