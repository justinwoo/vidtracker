module GenerateStylesheet where

import Prelude

import CSS (renderedSheet)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import FrontEnd.Style (stylesheet)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Text.Prettier (Parser(..), defaultOptions, format)

main :: Effect Unit
main = launchAff_ do
  case renderedSheet stylesheet of
    Just result
      | formatted <- format (defaultOptions {parser = CSS}) result -> do
      writeTextFile UTF8 "./dist/style.css" formatted
      liftEffect <<< log $ "rendered stylesheet"
    Nothing -> do
      liftEffect <<< error $ "error rendering stylesheet"
