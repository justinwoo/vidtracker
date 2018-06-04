module NameParser where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, lastIndexOf, splitAt)
import Data.String.CodeUnits (dropRight)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.String (char, regex)

type Parsed =
  { name :: String
  , episode :: String
  }

nameParser :: Parser Parsed
nameParser = do
  thingInBrackets
  _ <- char ' '
  title <- regex "[^\\[]+"
  thingInBrackets
  case splitAt <@> title <$> lastIndexOf (Pattern " - ") title of
    Just {before, after} | test (unsafeRegex "^ - \\d+ " noFlags) after ->
      pure { name: before, episode: dropRight 1 <<< drop 3 $ after }
    Just {before, after} ->
      fail $ "Invalid episode number: " <> after
    Nothing ->
      fail $ "Could not handle splitting of " <> title
  where
    thingInBrackets = do
      _ <- regex "\\[[^\\]]+\\]"
      pure unit
