module IdrisFMT

import Lightyear
import Lightyear.Core
import Lightyear.Strings
import Lightyear.Char

hex : Parser Int
hex = do
  c <- map (ord . toUpper) $ satisfy isHexDigit
  pure $ if c >= ord '0' && c <= ord '9' then c - ord '0'
                                         else 10 + c - ord 'A'

data Token =
  Import
  | Identifier String
  | Period
  | Spaces

Show Token where
  show Import = "Keyword(import)"
  show (Identifier x) = "Token(" ++ x ++ ")"
  show Period = "."
  show Spaces = "(space)"

importToken : Parser Token
importToken = do
  token <- string "import"
  pure $ Import

identifierToken : Parser Token
identifierToken = do
  token <- some ( satisfy isAlpha )
  pure $ Identifier $ pack token

periodToken : Parser Token
periodToken = map (\_ => Period) dot

spacesToken : Parser Token
spacesToken = map (\_ => Spaces) (some space)

tokenParser : Parser (List Token)
tokenParser = many (
  importToken <|>
  periodToken <|>
  identifierToken <|>
  spacesToken )
