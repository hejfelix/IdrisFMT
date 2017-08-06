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
  | Equality
  | LeftParen
  | RightParen
  | StringLiteral String
  | CharLiteral String
  | ListLiteral (List Token)
  | TupleLiteral (List Token)
  | IntegerLiteral String

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
  show Equality           = "="
  show LeftParen          = "("
  show RightParen         = ")"
  show (StringLiteral s)  = "\"" ++ s ++ "\""
  show (CharLiteral s)    = "'" ++ s ++ "'"
  show (ListLiteral xs)   = show xs
  show (TupleLiteral xs)  = show "(" ++ show xs ++ ")"
  show (IntegerLiteral i) = show i

escape : Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  pure $ pack $ (the $ List Char) [d,c]

nonEscape : Parser String
nonEscape = map show $ noneOf "\\\"\0\n\r\v\t\b\f"

character : Parser String
character = nonEscape <|> escape

stringLiteralToken : Parser Token
stringLiteralToken = map (StringLiteral . concat) $ dquote (many character)

charLiteralToken : Parser Token
charLiteralToken = map CharLiteral $ squote character

intLiteralToken : Parser Token
intLiteralToken = map (IntegerLiteral . show) integer

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
    ("|", Pipe),
    ("=", Equality),
    ("(", LeftParen),
    (")", RightParen)
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

identifierSymbols : List Char
identifierSymbols = [
 '!',
 '#',
 '$',
 '%',
 '&',
 '*',
 '+',
 '.',
 '/',
 '<',
 '=',
 '>',
 '?',
 '@',
 '\\',
 '^',
 ',',
 '-',
 '_',
 '~'
 ]



identifierToken : Parser Token
identifierToken = do
  first <- satisfy (\c => isAlpha c || hasAny [c] identifierSymbols)
  rest  <- many (satisfy (\c => isAlpha c || isDigit c || hasAny [c] identifierSymbols))
  pure $ Identifier $ (pack $ first :: rest)

mutual
  anyLiteral : Parser Token
  anyLiteral = stringLiteralToken
    <|> charLiteralToken
    <|> identifierToken
    <|> intLiteralToken
    <|>| listLiteralToken
    <|>| tupleLiteral

  tupleLiteral : Parser Token
  tupleLiteral = parens $ map TupleLiteral $ commaSep anyLiteral

  listLiteralToken : Parser Token
  listLiteralToken = brackets $ map ListLiteral $ commaSep anyLiteral

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
  anyLiteral         )

printFile : Either FileError String -> IO ()
printFile (Left l) = printLn (show l)
printFile (Right r) = printLn (parse tokenParser r)

main : IO ()
main = do
  maybeFileHandle <- readFile "IdrisFMT.idr"
  printFile maybeFileHandle
