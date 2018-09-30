module GetIcons where

import Prelude

import Config (Config)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Set (Set, fromFoldable, member)
import Data.String as S
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Effect.Exception as Exc
import NameParser (nameParser)
import Node.ChildProcess (defaultSpawnOptions, onClose, onError, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir)
import Simple.JSON as JSON
import Text.Parsing.StringParser (runParser)
import Toppokki as T
import Tortellini (parsellIni)

iconsPath :: String
iconsPath = "./dist/icons"

curl :: String -> String -> Aff Unit
curl url path = do
  cp <- liftEffect $ spawn "curl" ["-L", url, "-o", path] defaultSpawnOptions
  makeAff \cb -> do
    onError cp (cb <<< Left <<< Exc.error <<< show)
    onClose cp (cb <<< Right <<< const unit)
    pure mempty

downloadIconIfNotExist :: T.Browser -> Set String -> String -> Aff Unit
downloadIconIfNotExist browser existing name =
  unless (member name existing) do
    page <- T.newPage browser
    let
      name' = S.replace (S.Pattern " ") (S.Replacement "+") name
      pageURL = "https://duckduckgo.com/?iax=images&ia=images&q=" <> name' <> "+anime+wiki"
    T.goto (T.URL pageURL) page
    _ <- T.pageWaitForSelector (T.Selector ".tile--img__img") {} page
    result <- T.unsafeEvaluateStringFunction "document.querySelector('.tile--img__img').src" page
    case JSON.read result of
      Right (url :: String) -> do
        log $ "downloading from " <> url
        curl url (iconsPath <> "/" <> name)
        pure unit
      Left e -> do
        log $ "could not handle " <> name <> " with url " <> pageURL

main :: Effect Unit
main = launchAff_ do
  config <- parsellIni <$> readTextFile UTF8 "./config.ini"
  Aff.bracket
    (T.launch {})
    T.close
    (aff config)
  where
    aff config browser = do
      case config of
        (Right ({vidtracker: {dir}} :: Config)) -> do
          -- names :: Set String <- fromFoldable <$>
          --   bindFlipped (either (const mempty) pure <<< extractNameKinda <<< Path) <$>
          --   readdir dir
          paths <- readdir dir
          let
            results = Array.mapMaybe (hush <<< runParser nameParser) paths
            (names :: Set String) = fromFoldable (_.name <$> results)
          existing <- fromFoldable <$> readdir iconsPath
          for_ names $ downloadIconIfNotExist browser existing
          log $ "finished"
          pure unit
        Left e -> error $ "Error: " <> show e
