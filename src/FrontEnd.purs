module FrontEnd where

import Prelude
import Data.JSDate as JSDate
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D
import Network.HTTP.Affjax as AJ
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array ((:))
import Data.Either (Either(Left, Right))
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class IsForeign, readJSON, write)
import Data.HTTP.Method (Method(POST))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)
import Types (FileData(..), OpenRequest(..), Path, WatchedData(..))

type FE a = Either (NonEmptyList ForeignError) a
parseResponse :: forall t4 t7.
  (IsForeign t4) => { response :: String
                    | t7
                    }
                    -> Either (NonEmptyList ForeignError) t4
parseResponse response = runExcept $ readJSON response.response

type State =
  { files :: Array Path
  , watched :: Array WatchedData
  }

data Query a
  = Init a
  | OpenFile Path a
  | SetWatched Path Boolean a

type AppEffects eff =
  Aff
  ( ajax :: AJAX
  , console :: CONSOLE
  | eff )

ui :: forall eff. H.Component HH.HTML Query Unit Void (AppEffects eff)
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState =
      { files: mempty
      , watched: mempty
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
        $ HH.h1_ [ HH.text "Vidtracker" ]
          : (file <$> state.files)
      where
        file path =
          HH.div
            [ HP.class_ $ wrap "file"]
            [ HH.a
              [ HP.class_ $ wrap "file-link"
              , HE.onClick $ HE.input_ (OpenFile path) ]
              [ HH.text $ unwrap path ]
            , HH.button
              [ HP.classes $ wrap <$>
                [ "file-button"
                , "pure-button"
                , maybe "" (const "pure-button-primary") watched
                ]
              , HE.onClick $ HE.input_ (SetWatched path (not $ isJust watched))
              ]
              [ HH.text $ maybe "not watched" (const "watched") watched ]
            , HH.span
              [ HP.class_ $ wrap "file-note" ]
              [ HH.text $ maybe "" id watched ]
            ]
          where
            watched = getDate <$> find (\(WatchedData fd) -> fd.path == path) state.watched
            getDate (WatchedData {created}) = JSDate.toDateString <<< unsafePerformEff <<< JSDate.parse $ created

    eval :: Query ~> H.ComponentDSL State Query Void (AppEffects eff)
    eval (Init next) = do
      result <- getResult
      case result of
        Right (Tuple f w) ->
          H.modify \s -> s {files = f, watched = w}
        Left e ->
          H.liftAff $ error $ show e
      pure next
      where
        getJSON :: forall a. IsForeign a => String -> _ (FE a)
        getJSON url = H.liftAff $ parseResponse <$> AJ.get url
        getResult :: _ (FE (Tuple (Array Path) (Array WatchedData)))
        getResult =
          f
            <$> getJSON "/api/files"
            <*> getJSON "/api/watched"
          where
            f (Left a) (Left b) = Left $ a <> b
            f (Left e) _ = Left e
            f _ (Left e) = Left e
            f (Right a) (Right b) = Right $ Tuple a b

    eval (OpenFile path next) = do
      H.liftAff $ AJ.post_ "/api/open" $ toJSON (OpenRequest {path})
      pure next
      where
        toJSON = unsafeStringify <<< write

    eval (SetWatched path flag next) = do
      w :: FE (Array WatchedData) <- postJSON "/api/update" $ FileData {path, watched: flag}
      case w of
        Right watched ->
          H.modify \s -> s {watched = watched}
        Left e -> do
          let errors = show e
          pure unit
      pure next
      where
        postJSON url payload = H.liftAff $ parseResponse <$> post url (unsafeStringify $ write payload)
        post u c =
          AJ.affjax $ AJ.defaultRequest {method = Left POST, url = u, content = Just c}

main :: forall e.
  Eff
    ( avar :: AVAR
    , ref :: REF
    , err :: EXCEPTION
    , dom :: DOM
    , console :: CONSOLE
    , ajax :: AJAX
    | e
    )
    Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  log "Running"

