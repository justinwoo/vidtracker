module GenerateStylesheet where

import Prelude

import CSS (renderedSheet)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (error, log)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import FrontEnd.Style (stylesheet)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Text.Prettier (Parser(..), defaultOptions, format)

main :: Eff _ _
main = launchAff do
  case renderedSheet stylesheet of
    Just result
      | formatted <- format (defaultOptions {parser = PostCSS}) result -> do
      writeTextFile UTF8 "./dist/style.css" formatted
      log "rendered stylesheet"
    Nothing -> do
      error "error rendering stylesheet"
