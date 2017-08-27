module IdrisFMT

import Lightyear
import Lightyear.Core
import Lightyear.Strings
import Lightyear.Char

import Data.SortedMap

public export
data Token =
  Import
  | Module
  | Where
  | Identifier String
  | Period
  | Colon
  | Comma
  | Dollar
  | WhiteSpace Char
  | Newline
  | LeftArrow
  | RightArrow
  | BigRightArrow
  | Pipe
  | Equality
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | StringLiteral String
  | CharLiteral String
  | ListLiteral (List Token)
  | TupleLiteral (List Token)
  | IntegerLiteral Integer

export
[pretty] Show Token where
  show Module             = "module"
  show Import             = "import"
  show Where              = "where"
  show (Identifier x)     = x
  show Dollar             = "$"
  show Comma              = ","
  show Period             = "."
  show Colon              = ":"
  show LeftArrow          = "<-"
  show RightArrow         = "->"
  show BigRightArrow      = "=>"
  show (WhiteSpace c)     = the String $ cast c
  show Pipe               = "|"
  show Equality           = "="
  show LeftParen          = "("
  show RightParen         = ")"
  show LeftBrace          = "{"
  show RightBrace         = "}"
  show LeftBracket        = "["
  show RightBracket       = "]"
  show (StringLiteral s)  = "\"" ++ s ++ "\""
  show (CharLiteral s)    = "'" ++ s ++ "'"
  show (ListLiteral xs)   = "[" ++ foldl (++) "" (map show xs) ++ "]"
  show (TupleLiteral xs)  = "(" ++ foldl (++) "" (map show xs) ++ ")"
  show (IntegerLiteral i) = show i

export
[default] Show Token where
  show Module             = "module"
  show Import             = "import"
  show Where              = "where"
  show (Identifier x)     = "Identifier(" ++ x ++ ")"
  show Dollar             = "$"
  show Comma              = "(,)"
  show Period             = "."
  show Colon              = ":"
  show LeftArrow          = "<-"
  show RightArrow         = "->"
  show BigRightArrow      = "=>"
  show (WhiteSpace c)     = "(" ++ show c ++ ")"
  show Newline            = "(newline)"
  show Pipe               = "|"
  show Equality           = "="
  show LeftParen          = "("
  show RightParen         = ")"
  show LeftBrace          = "{"
  show RightBrace         = "}"
  show LeftBracket        = "["
  show RightBracket       = "]"
  show (StringLiteral s)  = "StringLiteral(\"" ++ s ++ "\")"
  show (CharLiteral s)    = "'" ++ s ++ "'"
  show (ListLiteral xs)   = unwords $ map show xs
  show (TupleLiteral xs)  = "(" ++ unwords (intersperse "," (map show xs)) ++ ")"
  show (IntegerLiteral i) = show i



escape : Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf'"
  pure $ pack $ (the $ List Char) [d,c]

nonEscape : Parser String
nonEscape = map (\x => pack $ (the $ List _) [x]) $ noneOf "\\\"\0\n\r\v\t\b\f"

character : Parser String
character = nonEscape <|>| escape

stringLiteralToken : Parser Token
stringLiteralToken = map (StringLiteral . concat) $ (between (char '"') (char '"')) (many character)

charLiteralToken : Parser Token
charLiteralToken = map CharLiteral $ (between (char '\'') (char '\'')) (character <|> (map singleton $ char '"'))

intLiteralToken : Parser Token
intLiteralToken = map IntegerLiteral integer

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
    (")", RightParen),
    ("{", LeftBrace),
    ("}", RightBrace),
    ("[", LeftBracket),
    ("]", RightBracket)
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

whiteSpaceToken : Parser Token
whiteSpaceToken = map WhiteSpace space

anyLiteral : Parser Token
anyLiteral = stringLiteralToken
  <|>| charLiteralToken
  <|>| identifierToken
  <|>| intLiteralToken
  <|>| stringLiteralToken

export
tokenParser : Parser (List Token)
tokenParser = many (
  keyWordToken      <|>
  identifierToken   <|>
  whiteSpaceToken   <|>
  anyLiteral         )

tokensToString : (Show Token) => List Token -> String
tokensToString xs = concat $ map show xs

printFile : Show Token => Either FileError String -> IO ()
printFile (Left l) = printLn (show l)
printFile (Right r) = case map tokensToString (parse tokenParser r) of
    (Left l) => putStrLn (show l)
    (Right r) => putStrLn r

main : IO ()
main = do
  _ <- putStrLn "Now parsing file..."
  maybeFileHandle <- readFile "IdrisFMT.idr"
  printFile @{pretty} maybeFileHandle

main2 : IO ()
main2 = putStrLn $ str
    where
      str = case parse tokenParser "[(\"hejabe\",  Abekat)]" of
                 (Left l) => "failed" ++ show l
                 (Right r) => show $ map (show @{pretty}) r
