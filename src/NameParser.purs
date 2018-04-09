module NameParser where

import Prelude

import Data.List (List)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (many1Till)
import Text.Parsing.StringParser.String (anyChar, anyDigit, char, string)

nameParser :: Parser (List Char)
nameParser = do
    thingInBrackets
    _ <- char ' '
    title <- many1Till anyChar endSequence
    pure title
  where
    thingInBrackets = do
      _ <- char '['
      _ <- many1Till anyChar (char ']')
      pure unit
    endSequence = try do
      _ <- string " - "
      _ <- many1Till anyDigit (char ' ')
      thingInBrackets
      _ <- char '.'
      pure unit
