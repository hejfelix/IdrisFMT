module IdrisFMT

import Text.Lexer
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
  | WhiteSpace String
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
  | IntegerLiteral String
  | UnknownToken String
  | Comment String

-- export
-- [pretty] Show Token where
--   show Module             = "module"
--   show Import             = "import"
--   show Where              = "where"
--   show (Identifier x)     = x
--   show Dollar             = "$"
--   show Comma              = ","
--   show Period             = "."
--   show Colon              = ":"
--   show LeftArrow          = "<-"
--   show RightArrow         = "->"
--   show BigRightArrow      = "=>"
--   show (WhiteSpace c)     = the String $ cast c
--   show Pipe               = "|"
--   show Equality           = "="
--   show LeftParen          = "("
--   show RightParen         = ")"
--   show LeftBrace          = "{"
--   show RightBrace         = "}"
--   show LeftBracket        = "["
--   show RightBracket       = "]"
--   show (StringLiteral s)  = "\"" ++ s ++ "\""
--   show (CharLiteral s)    = "'" ++ s ++ "'"
--   show (ListLiteral xs)   = "[" ++ foldl (++) "" (map show xs) ++ "]"
--   show (TupleLiteral xs)  = "(" ++ foldl (++) "" (map show xs) ++ ")"
--   show (IntegerLiteral i) = show i
--   show (UnknownToken s)   = s

Show a => Show (TokenData a) where
  show (MkToken line col tok) =
    "(" ++ (show line)
      ++ ", "
      ++ (show col)
      ++ ": "
      ++ (show tok) ++ ")"



export
Show Token where
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
  show (WhiteSpace s)     = "(" ++ s ++ ")"
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
  show (IntegerLiteral s) = s
  show (UnknownToken s)   = "UnknownToken: " ++ show s
  show (Comment s)        = s

keywordTokens : List (String, Token)
keywordTokens = [
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

keyWordMap : SortedMap String Token
keyWordMap = fromList keywordTokens

keywordLexers : List Lexer
keywordLexers = map toLexer keywordTokens
  where
    toLexer (keyword, _) = exact keyword

keywordMapper : String -> Token
keywordMapper tokenString = fromMaybe (UnknownToken tokenString) $ lookup tokenString keyWordMap

keyWordTokenizer : List (Lexer, String -> Token)
keyWordTokenizer = map (\lexer => (lexer, keywordMapper)) keywordLexers

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


identifierLexer : Lexer
identifierLexer = do
  (pred (\c => isAlpha c || isDigit c || hasAny [c] identifierSymbols))
    <+> (many $ pred (\c => isAlpha c || isDigit c || hasAny [c] identifierSymbols))

identifierTokenizer : (Lexer, String -> Token)
identifierTokenizer = (identifierLexer, Identifier)

anyLiteralTokenizer : List (Lexer, String -> Token)
anyLiteralTokenizer =
  [
    (space, WhiteSpace),
    (stringLit, StringLiteral),
    (intLit, IntegerLiteral),
    (charLit, CharLiteral),
    (lineComment (exact "--"), Comment)
  ]

tokensToString : (Show Token) => List Token -> String
tokensToString xs = concat $ map show xs

tokenMap : TokenMap Token
tokenMap = anyLiteralTokenizer ++ identifierTokenizer :: keyWordTokenizer

doLex : String -> String
doLex s = case lex tokenMap s of
  (tokens, line, col, remain) => unlines $ map show tokens

printFile : Show Token => Either FileError String -> IO ()
printFile (Left l) = printLn ("Error reading file" ++ show l)
printFile (Right r) = do
  _ <- putStrLn "tokenizing..."
  _ <- putStrLn $ doLex r
  putStrLn "finished..."

lineTokenMap : TokenMap Token
lineTokenMap = [(lineComment (exact "--"), Comment)]

main : IO ()
main = do
  _ <- putStrLn "Now parsing file..."
  maybeFileHandle <- readFile "IdrisFMT.idr"
  printFile maybeFileHandle
--
-- main2 : IO ()
-- main2 = putStrLn $ str
--     where
--       str = case parse tokenParser "[(\"hejabe\",  Abekat)]" of
--                  (Left l) => "failed" ++ show l
--                  (Right r) => show $ map (show @{pretty}) r
