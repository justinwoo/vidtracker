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
import Data.Array (filter, reverse, sort, sortWith)
import Data.Either (Either(Left), either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Encode, class Decode, encode)
import Data.Foreign.Generic (decodeJSON)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..), contains, toLower)
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)
import Routes (Route(..), files, open, update, watched)
import Types (FileData(..), OpenRequest(..), Path(..), WatchedData(..))

type VE a = V (NonEmptyList ForeignError) a

request :: forall req res m eff.
  MonadAff
    ( ajax :: AJAX
    | eff
    )
    m
  => Encode req
  => Decode res
  => Route req res -> Maybe req -> m (VE res)
request (Route route) body =
  H.liftAff $ either invalid pure <$> parseResponse <$> action
  where
    action = AJ.affjax $ AJ.defaultRequest
      { method = Left route.method
      , url = route.url
      , content = unsafeStringify <<< encode <$> body
      }
    parseResponse response =
      runExcept $ decodeJSON response.response

unV' :: forall e a m.
  MonadAff
    ( console :: CONSOLE
    | e
    )
    m
  => (a -> m Unit) -> VE a -> m Unit
unV' = unV (H.liftAff <<< errorShow)

data Dir = ASC | DSC
derive instance eqDir :: Eq Dir
data Col = Title | Status
derive instance eqCol :: Eq Col
data Sorting = Sorting Col Dir | NoSorting

type State =
  { files :: Array Path
  , watched :: Array WatchedData
  , sorting :: Sorting
  , search :: String
  }

data Query a
  = Init a
  | OpenFile Path a
  | SetWatched Path Boolean a
  | ChangeSorting Col a
  | Search String a
  | ClearSearch a

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
      , sorting: NoSorting
      , search: mempty
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
        $ [ HH.h1_ [ HH.text "Vidtracker" ]
          , HH.div
            [ HP.class_ $ wrap "search" ]
            $ [ HH.h4_ [ HH.text "Search" ]
              , HH.input
                  [ HP.value state.search
                  , HE.onValueInput (HE.input Search)
                  ]
              ] <> if state.search == ""
                then mempty
                else pure $
                  HH.button
                    [ HE.onClick $ HE.input_ ClearSearch ]
                    [ HH.text "Clear" ]
          , HH.div
            [ HP.class_ $ wrap "file"]
            [ HH.h3
              [ HP.class_ $ wrap "file-link"
              , HE.onClick $ HE.input_ (ChangeSorting Title)
              ] [ HH.text $ "Title" <> displayTicker Title ]
            , HH.h3
              [ HP.class_ $ wrap "file-button"
              , HE.onClick $ HE.input_ (ChangeSorting Status)
              ] [ HH.text $ "Status" <> displayTicker Status ]
            , HH.h3 [HP.class_ $ wrap "file-note"] [ HH.text "Date" ]
            ]
          ] <> (file <$> applyTransforms state.files)
      where
        displayTicker col
          | Sorting col' dir <- state.sorting
          , asc <- dir == ASC
          , col' == col = if asc
            then " ASC"
            else " DSC"
          | otherwise = ""
        applyTransforms = applySorting <<< applyFiltering
        applyFiltering = case state.search of
            "" -> id
            x -> filter $ \(Path path) -> contains (Pattern $ toLower x) (toLower path)
        applySorting
          | Sorting col dir <- state.sorting
          , rev <- if dir == ASC then id else reverse
          , sort' <- case col of
            Title -> sort
            Status -> sortWith findWatched
          = rev <<< sort'
          | otherwise = id
        findWatched path = find (\(WatchedData fd) -> fd.path == path) state.watched
        file path =
          HH.div
            [ HP.class_ $ wrap "file"]
            [ HH.a
              [ HP.classes $ wrap <$>
                [ "file-link"
                , maybe "" (const "watched") watched
                ]
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
            watched = getDate <$> findWatched path
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
      _ <- request open $ Just (OpenRequest {path})
      pure next

    eval (SetWatched path flag next) = do
      request update (Just $ FileData {path, watched: flag}) >>= unV'
        \w -> H.modify _ {watched = w}
      pure next

    eval (Search str next) = do
      H.modify _ {search = str}
      pure next

    eval (ClearSearch next) = do
      H.modify _ {search = ""}
      pure next

    eval (ChangeSorting col next)= do
      H.modify $ \s -> case s.sorting of
        Sorting curr dir | curr == col ->
          s {sorting = case dir of
              ASC -> Sorting col DSC
              DSC -> NoSorting
            }
        _ -> s {sorting = Sorting col ASC}
      pure next

main :: forall e.
  Eff
    ( avar :: AVAR
    , ref :: REF
    , exception :: EXCEPTION
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

