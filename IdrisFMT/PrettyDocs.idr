module IdrisFMT.PrettyDocs

import IdrisFMT
import Lightyear
import Lightyear.Strings

import Text.PrettyPrint.WL

dataDefinitionExample : String
dataDefinitionExample =
  """data Token =
    Import
    | Module | Where
  | Identifier String
  |    Period
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
                        | ListLiteral (List Token)
        | TupleLiteral (List Token)
  | CharLiteral String
          | IntegerLiteral Integer"""

putList : Show a => (List a) -> IO ()
putList xs = putStrLn (show (map show xs))

putTokens : Show Token => (List Token) -> IO ()
putTokens xs = traverse_ putStrLn (map show xs)

isPipe : Token -> Bool
isPipe Pipe   = True
isPipe _      = False

isEquality : Token -> Bool
isEquality Equality = True
isEquality _ = False

record DataDefinition where
  constructor MkDataDefinition
    header, body : List Token

dataDef : List Token -> DataDefinition
dataDef xs = case List.span (not . isPipe) xs of
  (before, after) => MkDataDefinition before (drop 1 after)

dropLeadingWhiteSpace : List Token -> List Token
dropLeadingWhiteSpace [] = []
dropLeadingWhiteSpace ((WhiteSpace ' ') :: xs) = dropLeadingWhiteSpace xs
dropLeadingWhiteSpace ((WhiteSpace '\n') :: xs) = dropLeadingWhiteSpace xs
dropLeadingWhiteSpace (x :: xs) = WhiteSpace ' ' :: x :: xs

tokenToDoc : Token -> Doc
tokenToDoc (WhiteSpace '\n') = empty
tokenToDoc tk                = text $ show @{pretty} tk

tokensToDoc : List Token -> Doc
tokensToDoc xs = foldl (|+|) empty $ map tokenToDoc (dropLeadingWhiteSpace xs)

bodyToDoc : List Token -> Doc
bodyToDoc tokens = cat $ map tokensToDoc $ map (((::) Pipe) . dropLeadingWhiteSpace) (List.split isPipe tokens)


dataDefToDoc : DataDefinition -> Doc
dataDefToDoc (MkDataDefinition header body) =
  headDoc |+| (group $ nest 4 (line |+| (bodyToDoc body)))
    where
      headDoc = case List.span (not . isEquality) header of
        (before, after) => (tokensToDoc before) |+| text "=" |+| (tokensToDoc $ dropLeadingWhiteSpace $ drop 1 after)


prettyTokensStr : List Token -> String
prettyTokensStr tokens = foldl (++) "" $ map (show @{pretty}) tokens

defaultTokenStr : List Token -> String
defaultTokenStr tokens = foldl (++) "" $ map (show @{default}) tokens

runExample : String -> Int -> IO ()
runExample code maxWidth = case (Strings.parse tokenParser code) of
  Right(tokens) => putStrLn $ toString 0 maxWidth $ dataDefToDoc (dataDef tokens)
  Left(err)     => putStrLn err
