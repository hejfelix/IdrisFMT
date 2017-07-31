module IdrisFMT

import Lightyear
import Lightyear.Core
import Lightyear.Strings
import Lightyear.Char

import Data.SortedMap

data Token =
  Import
  | Module
  | Where
  | Identifier String
  | Period
  | Colon
  | Comma
  | Dollar
  | Spaces
  | Newline
  | LeftArrow
  | RightArrow
  | BigRightArrow
  | Pipe
  | StringLiteral String

Show Token where
  show Module             = "module"
  show Import             = "import"
  show Where              = "where"
  show (Identifier x)     = "Identifier(" ++ x ++ ")"
  show Dollar             = "$"
  show Comma              = ","
  show Period             = "."
  show Colon              = ":"
  show LeftArrow          = "<-"
  show RightArrow         = "->"
  show BigRightArrow      = "=>"
  show Spaces             = "(space)"
  show Newline            = "(newline)"
  show Pipe               = "|"
  show (StringLiteral s)  = "\"" ++ s ++ "\""


keyWordMap : SortedMap String Token
keyWordMap = fromList
  [
    ("module", Module),
    ("import", Import),
    ("where", Where),
    ("<-", LeftArrow),
    ("->", RightArrow),
    ("=>", BigRightArrow),
    (".", Period),
    (",", Comma),
    (":", Colon),
    ("$", Dollar),
    ("|", Pipe)
  ]

emptyParser : ParserT String Identity String
emptyParser = fail "emptyParser"

keyWordStringParser : Parser String
keyWordStringParser = foldl (<|>) emptyParser $ map (string . fst) $ SortedMap.toList keyWordMap

keyWordToken : Parser Token
keyWordToken = do
  keyWordString <- keyWordStringParser
  pure (case SortedMap.lookup keyWordString keyWordMap of
        Nothing => StringLiteral $ "no token for keyword: " ++ keyWordString
        (Just x) => x)

importToken : Parser Token
importToken = do
  token <- string "import"
  pure $ Import

whereToken : Parser Token
whereToken = do
  token <- string "where"
  pure $ Import

moduleToken : Parser Token
moduleToken = do
  token <- string "module"
  pure $ Module

identifierToken : Parser Token
identifierToken = do
  token <- some ( satisfy isAlpha )
  pure $ Identifier $ pack token

stringLiteralToken : Parser Token
stringLiteralToken = map StringLiteral $ quoted '"'

spacesToken : Parser Token
spacesToken = map (\_ => Spaces) (some space)

newlineToken : Parser Token
newlineToken = map (\_ => Newline) newline


tokenParser : Parser (List Token)
tokenParser = many (
  keyWordToken      <|>
  identifierToken   <|>
  spacesToken       <|>
  newlineToken      <|>
  stringLiteralToken)
