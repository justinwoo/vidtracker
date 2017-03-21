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
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, errorShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Array ((:))
import Data.Either (Either(Left), either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class AsForeign, class IsForeign, readJSON, write)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)
import Routes (Route(..), files, open, update, watched)
import Types (FileData(..), OpenRequest(..), Path, WatchedData(..))

type VE a = V (NonEmptyList ForeignError) a

request :: forall req res m eff.
  ( MonadAff
      ( ajax :: AJAX
      | eff
      )
      m
  , AsForeign req
  , IsForeign res
  ) => Route req res -> Maybe req -> m (VE res)
request (Route route) body =
  H.liftAff $ either invalid pure <$> parseResponse <$> action
  where
    action = AJ.affjax $ AJ.defaultRequest
      { method = Left route.method
      , url = route.url
      , content = unsafeStringify <<< write <$> body
      }
    parseResponse response =
      runExcept $ readJSON response.response

unV' :: forall e a m.
  ( MonadAff
    ( console :: CONSOLE
    | e
    )
    m
  ) => (a -> m Unit) -> VE a -> m Unit
unV' = unV (H.liftAff <<< errorShow)

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
      getResult >>= unV'
        \(Tuple f w) -> H.modify _ {files = f, watched = w}
      pure next
      where
        getResult = do
          files <- request files Nothing
          watched <- request watched Nothing
          pure $ Tuple <$> files <*> watched

    eval (OpenFile path next) = do
      request open $ Just (OpenRequest {path})
      pure next

    eval (SetWatched path flag next) = do
      request update (Just $ FileData {path, watched: flag}) >>= unV'
        \w -> H.modify _ {watched = w}
      pure next

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

