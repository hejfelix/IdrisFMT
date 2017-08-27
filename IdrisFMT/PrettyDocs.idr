module IdrisFMT.PrettyDocs

import IdrisFMT
import Lightyear
import Lightyear.Strings

import Text.PrettyPrint.WL

dataDefinitionExample : String
dataDefinitionExample =
  """data Token =
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
          | IntegerLiteral Integer"""

putList : Show a => (List a) -> IO ()
putList xs = traverse_ putStrLn (map show xs)

putTokens : Show Token => (List Token) -> IO ()
putTokens xs = traverse_ putStrLn (map show xs)

runExample : IO ()
runExample = case (Strings.parse tokenParser dataDefinitionExample) of
  Right(tokens) => putTokens @{default} tokens
  Left(err)     => putStrLn err
