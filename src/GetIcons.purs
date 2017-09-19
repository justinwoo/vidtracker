module GetIcons where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Aff.Console (error, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exc
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (find, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Set (Set, fromFoldable, member)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import FrontEnd (extractNameKinda)
import Global.Unsafe (unsafeStringify)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Network.HTTP.Affjax (get)
import Node.ChildProcess (defaultSpawnOptions, onClose, onError, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Node.Process (lookupEnv)
import Simple.JSON (readJSON)
import Types (Path(..))

type Config =
  { queryUrl :: String
  }

readConfig :: Aff _ (Either _ Config)
readConfig = do
  contents <- readTextFile UTF8 "./icons-config.json"
  pure $ runExcept $ readJSON contents

iconsPath :: String
iconsPath = "./dist/icons"

curl :: forall e.
  String
  -> String
  -> Aff
      _
      Unit
curl url path = do
  cp <- liftEff $ spawn "curl" [url, "-o", path] defaultSpawnOptions
  makeAff \e s -> do
    onError cp (e <<< Exc.error <<< unsafeStringify)
    onClose cp (s <<< const unit)

downloadIconIfNotExist :: Config -> Set String -> String -> Aff _ Unit
downloadIconIfNotExist config existing name =
  unless (member name existing) do
    log $ "gonna get " <> name
    html <- _.response <$> get (config.queryUrl <> name)
    case extractFirstImage =<<
      (lmap (append "couldn't get tags " <<< show) (parseTags html)) of
      Right url -> do
        log $ "downloading from " <> url
        curl url (iconsPath <> "/" <> name)
        pure unit
      Left e ->
        error e
  where
    matchSrc (Attribute (Name name) _) = name == "src"
    extractFirstImage tags =
      case tags of
        -- $('article > div > div > a > img')
        (TagOpen (TagName "article") _)
          : (TagOpen (TagName "div") _)
          : (TagOpen (TagName "div") _)
          : (TagOpen (TagName "a") _)
          : (TagOpen (TagName "img") attrs)
          : xs
          | Just (Attribute _ (Value src)) <- find matchSrc attrs
          -> Right src
        (_ : xs) -> extractFirstImage xs
        mempty -> Left "couldn't find image???"

main :: Eff _ _
main = launchAff do
  dir <- liftEff $ lookupEnv "FILETRACKER_DIR"
  config <- readConfig

  case Tuple dir config of
    Tuple (Just dir') (Right config') -> do
      names :: Set String <- fromFoldable <$>
        bindFlipped (either (const mempty) pure <<< extractNameKinda <<< Path) <$>
        readdir dir'
      existing <- fromFoldable <$> readdir iconsPath
      for_ names $ downloadIconIfNotExist config' existing
      pure unit
    Tuple Nothing _ -> error "env var not set?"
    Tuple _ (Left e) -> error $ show e
