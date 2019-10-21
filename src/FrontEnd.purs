module FrontEnd where

import Prelude

import ChocoPie (runChocoPie)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import FrontEnd.HTTP (http)
import FrontEnd.Types (Action(..), AppState, DateString(..), Direction(..), File, KeyboardEvent(..), Request(..), ScrollEvent(..))
import FrontEnd.UI (view)
import FrontEnd.Window (window)
import NameParser (nameParser)
import Text.Parsing.StringParser (runParser)
import Types (Path(..), WatchedData)

initialState :: AppState
initialState =
  { files: []
  , paths: []
  , watchedData: []
  , filesLoading: false
  , iconsLoading: false
  , grouped: false
  , filterWatched: false
  , cursor: Nothing
  }

fold' :: forall a b. (b -> a -> a) -> Event b -> a -> Event a
fold' fn event init = pure init <|> Event.fold fn event init

main :: Effect Unit
main = do
  request <- Event.create

  let
    mkSink sources =
      let
        actions
            = map EEQuery sources.window
          <|> map Response sources.http
          <|> sources.view

        appState = fold' update actions initialState
        actionsWithAppState = Event.sampleOn appState (Tuple <$> actions)

      in
        { window: getScrollEvents actionsWithAppState
        , view: appState
        , http: request.event <|> getActionRequests actionsWithAppState
        }

    drivers = {window, view, http}

  liftEffect $ runChocoPie mkSink drivers

  -- initial data request
  request.push FetchFilesRequest

  Console.log "Started application"

calculateFiles :: AppState -> AppState
calculateFiles state =
  let
    { filterWatched, grouped, paths, watchedData } = state

    modifiers = { filterWatched, grouped }
    allFiles = getFiles paths watchedData
    files = processFiles allFiles modifiers

  in state { files = files }

update :: Action -> AppState -> AppState
update action state = case action of
  FetchFiles -> state { filesLoading = true }

  EEQuery FetchIconsEvent -> state { iconsLoading = true }
  EEQuery RefreshEvent -> state { filesLoading = true }
  EEQuery ToggleGroupedEvent -> calculateFiles $ state { grouped = not state.grouped }
  EEQuery ToggleFilterWatched -> calculateFiles $ state { filterWatched = not state.filterWatched }

  Response { filesData, watchedData } -> calculateFiles $
    state
      { filesLoading = false
      , iconsLoading = false
      , watchedData = watchedData
      , paths = filesData
      }

  EEQuery (DirectionEvent Up) -> state
    { cursor =
        case state.cursor of
          Just idx -> Just $ max 0 (idx - 1)
          Nothing -> Just 0
    }

  EEQuery (DirectionEvent Down) -> state
    { cursor =
        case state.cursor of
          Just idx -> Just $ min (Array.length state.files - 1) (idx + 1)
          Nothing -> Just 0
    }

  SetCursor i -> state { cursor = Just i }
  LinkClick i _ -> state { cursor = Just i }

  OpenFile -> state
  MarkFile -> state
  ToggleWatched _ -> state
  WatchedClick _ -> state
  EEQuery OpenEvent -> state
  EEQuery MarkEvent -> state

processFiles :: Array File -> { filterWatched :: Boolean, grouped :: Boolean } -> Array File
processFiles files flags = applyFilter $ applyGrouping files
  where
    applyGrouping = if flags.grouped
      then Array.sortBy (on compare _.series <> on compare _.episode)
      else identity

    applyFilter = if flags.filterWatched
      then Array.filter (isNothing <<< _.watched)
      else identity

getFiles :: Array Path -> Array WatchedData -> Array File
getFiles paths watchedData = do
  annotateLatest $ mkFile watchedData <$> paths
  where
    mkFile watched file@(Path name) =
      { name: Path name
      , watched: DateString <<< _.created <$> Array.find (\x -> x.path == file) watched
      , series: _.name <$> parsed
      , episode: Int.fromString <<< _.episode =<< parsed
      , latest: Nothing
      } :: File
      where
        parsed = hush $ runParser nameParser name

    annotateLatest :: Array File -> Array File
    annotateLatest xs = updateLatest <$> xs
      where
        watchedFiles :: Array File
        watchedFiles = Array.filter (\x -> isJust x.watched) xs

        grouped :: Array (Maybe File)
        grouped
            = maximumBy (compare `on` _.episode)
          <$> Array.groupBy (eq `on` _.series)
              (Array.sortWith _.series watchedFiles)
        updateLatest x
          | match' <- \z -> z.series == x.series
          , match <- \y -> maybe false match' y
          , Just (Just y) <- Array.find match grouped
            = x { latest = y.episode }
          | otherwise = x

getScrollEvents :: Event (Tuple Action AppState) -> Event ScrollEvent
getScrollEvents = Event.filterMap go
  where
    go (Tuple action state) =
      let
        fileAtCursor = state.cursor >>= Array.index state.files
      in case fileAtCursor, action of
        Just file, EEQuery (DirectionEvent _) -> Just (ScrollFileIntoView file)
        _, _ -> Nothing

getActionRequests :: Event (Tuple Action AppState) -> Event Request
getActionRequests = Event.filterMap go
  where
    go (Tuple action state) =
      let
        fileAtCursor = state.cursor >>= Array.index state.files
      in case fileAtCursor, action of
        _, FetchFiles -> Just FetchFilesRequest

        Just file, OpenFile -> Just (OpenFileRequest file)
        _, LinkClick _ file -> Just (OpenFileRequest file)
        Just file, EEQuery OpenEvent -> Just (OpenFileRequest file)

        Just file, MarkFile -> Just (MarkFileRequest file)
        Just file, ToggleWatched path -> Just (MarkFileRequest file)
        Just file, EEQuery MarkEvent  -> Just (MarkFileRequest file)

        _, EEQuery RefreshEvent -> Just FetchFilesRequest
        _, EEQuery FetchIconsEvent  -> Just FetchIconsRequest

        _, _ -> Nothing
