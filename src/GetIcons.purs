module GetIcons where

import Prelude

import Config (IconsConfig, Config)
import Control.Bind (bindFlipped)
import Control.Monad.Aff (Aff, launchAff_, makeAff)
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exc
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (find, (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Set (Set, fromFoldable, member)
import Data.Traversable (for_)
import FrontEnd (extractNameKinda)
import Global.Unsafe (unsafeStringify)
import LenientHtmlParser (Attribute(..), Name(..), Tag(..), TagName(..), Value(..), parseTags)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.ChildProcess (CHILD_PROCESS, defaultSpawnOptions, onClose, onError, spawn)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile, readdir)
import Node.Process (PROCESS)
import Tortellini (parseIni)
import Types (Path(..))

iconsPath :: String
iconsPath = "./dist/icons"

curl :: forall e.
  String
  -> String
  -> Aff
      ( cp :: CHILD_PROCESS
      | e
      )
      Unit
curl url path = do
  cp <- liftEff $ spawn "curl" [url, "-o", path] defaultSpawnOptions
  makeAff \cb -> do
    onError cp (cb <<< Left <<< Exc.error <<< unsafeStringify)
    onClose cp (cb <<< Right <<< const unit)
    pure mempty

downloadIconIfNotExist :: forall e
   . IconsConfig
  -> Set String
  -> String
  -> Aff
       ( console :: CONSOLE
       , cp :: CHILD_PROCESS
       | e
       )
       Unit
downloadIconIfNotExist config existing name =
  unless (member name existing) do
    log $ "gonna get " <> name
    html <- M.text =<< M.fetch nodeFetch (M.URL $ config.queryUrl <> name) M.defaultFetchOptions
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

main :: forall e.
  Eff
    ( fs :: FS
    , console :: CONSOLE
    , cp :: CHILD_PROCESS
    , process :: PROCESS
    | e
    )
    Unit
main = launchAff_ do
  config <- parseIni <$> readTextFile UTF8 "./config.ini"

  case config of
    (Right ({vidtracker: {dir}, icons: config'} :: Config)) -> do
      names :: Set String <- fromFoldable <$>
        bindFlipped (either (const mempty) pure <<< extractNameKinda <<< Path) <$>
        readdir dir
      existing <- fromFoldable <$> readdir iconsPath
      for_ names $ downloadIconIfNotExist config' existing
      log "finished"
      pure unit
    Left e -> error $ "Error: " <> show e
