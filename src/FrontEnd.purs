module FrontEnd where

import Prelude

import ChocoPie (runChocoPie)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (intercalate, maximumBy)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1)
import FRP.Event (Event)
import FRP.Event as Event
import Global.Unsafe (unsafeEncodeURIComponent)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import NameParser (nameParser)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.DOM.Events as RE
import React.Basic.Events (SyntheticEvent)
import React.Basic.Events as Events
import Routes (GetRoute, PostRoute, apiRoutes)
import Simple.JSON as JSON
import Text.Parsing.StringParser (runParser)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Types (Path(..), WatchedData)

newtype DateString = DateString String
derive instance newtypeDateString :: Newtype DateString _

type File =
  { name :: Path
  , watched :: Maybe DateString
  , series :: Maybe String
  , episode :: Maybe Int
  , latest :: Maybe Int
  }

type State =
  -- files
  { files :: Array File
  , filesData :: Array Path
  , watchedData :: Array WatchedData
  , filesLoading :: Boolean
  , iconsLoading :: Boolean
  , grouped :: Boolean
  -- counted by positions from the top
  , cursor :: Maybe Int
  }

data Action
  = FetchData
  | UpdateFiles
  | OpenFile
  | MarkFile
  | ToggleWatched Path
  | SetCursor Int
  | LinkClick Int
  | WatchedClick Int
  | EEQuery ExternalEvent

initialState :: State
initialState =
  { files: []
  , filesData: []
  , watchedData: []
  , filesLoading: false
  , iconsLoading: false
  , grouped: false
  , cursor: Nothing
  }

type Props = {}

type Self = RB.Self Props State

div' :: String -> Array RB.JSX -> RB.JSX
div' className children = R.div { className, children }

capture_ :: Aff Unit -> EffectFn1 SyntheticEvent Unit
capture_ = RE.capture_ <<< launchAff_

handler_ :: Aff Unit -> EffectFn1 SyntheticEvent Unit
handler_ = Events.handler_ <<< launchAff_

readState' :: Self -> Aff State
readState' = liftEffect <<< RB.readState

mkIconURL :: String -> String
mkIconURL series = "url(\"" <> un M.URL (prefixUrl $ iconsPath) <> "\")"
  where iconsPath = "/icons/" <> unsafeEncodeURIComponent series

render :: Self -> RB.JSX
render self@{state} =
  R.div_
    [ R.h1_ [R.text "vidtracker"]
    , header
    , R.div_ $ Array.mapWithIndex mkFile files
    ]
  where
    files = if state.grouped
      then Array.sortBy
        (\x y -> compare x.series y.series <> compare x.episode y.episode)
        state.files
      else state.files

    header = div' "header"
      [ div' "info" $ [ R.h3_ [ R.text "Info:" ] ] <> infoLines
      , div' "recents" $ [ R.h3_ [ R.text "Recently watched:" ] ] <> recents
      ]

    infoLines = R.span_ <<< pure <<< R.text <$>
      [ "o: open current file"
      , "k: move cursor up"
      , "j: move cursor down"
      , "W/M: mark as watched"
      , "r: refresh"
      , "I: fetch icons and reload page"
      , "files loading: " <> if state.filesLoading then "true" else "false"
      , "icons loading: " <> if state.iconsLoading then "true" else "false"
      , "grouped by series: " <> if state.grouped then "true" else "false"
      ]

    recents = mkRecent <$> Array.take (Array.length infoLines) state.watchedData

    mkRecent { path: Path name } = div' "recent" [ R.text name ]

    mkFile :: Int -> File -> _
    mkFile idx file = R.div
      { className: intercalate " "
          [ "file"
          , case state.cursor of
              Just pos | pos == idx -> "cursor"
              _ -> ""
          , case file.watched of
              Just _ -> "done"
              Nothing -> ""
          ]
      , title : "latest watched: " <> fromMaybe "unknown" (show <$> file.latest)
      , onClick: handler_ $ eval self (SetCursor idx)
      , children:
          [ R.div
              { className: "icon"
              , style:
                  case file.series of
                    Just series -> R.css { backgroundImage: mkIconURL series }
                    Nothing -> R.css {}
              }
          , R.div
              { className: "name"
              , title: (un Path file.name)
              , children: [ R.text $ un Path file.name ]
              , onClick: capture_ $ eval self (LinkClick idx)
              }
          , R.div
              { className: intercalate " "
                [ "watched"
                , maybe "" (const "has-date") file.watched
                ]
              , onClick: capture_ $ eval self (WatchedClick idx)
              , children:
                  [ R.text $ case file.watched of
                      Just (DateString date) -> "watched " <> date
                      Nothing -> maybe "" (\x -> " last: " <> show x) file.latest
                  ]
              }
        ]
    }

eval :: Self -> Action -> Aff Unit

eval self (LinkClick idx) = do
  eval self (SetCursor idx)
  eval self OpenFile

eval self (WatchedClick idx) = do
  eval self (SetCursor idx)
  eval self MarkFile

eval self FetchData = do
  setStateAff self _ { filesLoading = true }
  attempt <- getData
  liftEffect $ case attempt of
    Right r -> self.setState _
      { filesData = r.filesData
      , watchedData = r.watchedData
      , filesLoading = false
      }
    Left e -> Console.error $ "Failed to fetch data: " <> show e
  eval self UpdateFiles
  where
    getData = do
      filesData <- get apiRoutes.files
      watchedData <- get apiRoutes.watched
      pure $ { filesData: _, watchedData: _ }
        <$> filesData
        <*> watchedData

eval self UpdateFiles = do
  setStateAff self \s -> do
    let files = mkFile s.watchedData <$> s.filesData
    let annotated = annotateLatest files
    if s.grouped
      then s { files = groupFiles annotated }
      else s { files = annotated }
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

    groupFiles = Array.sortBy
      (\x y
        -> compare x.series y.series
        <> compare x.episode y.episode
      )

eval self (ToggleWatched name) = do
  state <- readState' self
  Console.log $ "Updating " <> un Path name
  case Array.find (\x -> x.name == name) state.files of
    Just file -> do
      _ <- post apiRoutes.update
        { path: file.name
        , watched: maybe true (const false) file.watched
        }
      pure unit
    Nothing -> Console.error $ "Could not find file named " <> un Path name
  eval self FetchData

eval self (SetCursor idx) = do
  setStateAff self _ { cursor = Just idx }

eval self (EEQuery (DirectionEvent dir)) = do
  liftEffect case dir of
    Down -> self.setState \s -> do
      let cursor = min (Array.length s.files) (maybe 0 (_ + 1) s.cursor)
      s { cursor = Just cursor }
    Up -> self.setState \s -> do
      let cursor = max 0 (maybe 0 (_ - 1) s.cursor)
      s { cursor = Just cursor }

  newState <- liftEffect $ RB.readState self
  case newState.cursor of
    Just cursor | cursor == 0 -> liftEffect $ scrollToTop
    Just cursor | Just file <- Array.index newState.files =<< newState.cursor -> do
      liftEffect $ scrollIntoView file.name
    _ -> pure unit

eval self OpenFile = do
  state <- readState' self
  case state.cursor of
    Just pos
      | Just file <- Array.index state.files pos
      -> do
      Console.log $ "Opening file: " <> un Path file.name
      _ <- post apiRoutes.open { path: file.name }
      pure unit
    _ -> pure unit

eval self (EEQuery OpenEvent) = eval self OpenFile

eval self MarkFile = do
  state <- readState' self
  case state.cursor of
    Just pos
      | Just file <- Array.index state.files pos
      -> eval self (ToggleWatched file.name)
    _ -> pure unit

eval self (EEQuery MarkEvent) = eval self MarkFile

eval self (EEQuery RefreshEvent) = do
  Console.log "RefreshEvent"
  eval self FetchData

eval self (EEQuery FetchIconsEvent) = do
  Console.log "FetchIconsEvent"
  setStateAff self _ { iconsLoading = true }
  result <- post apiRoutes.getIcons {}
  setStateAff self _ { iconsLoading = false }
  liftEffect $ refreshPage

eval self (EEQuery ToggleGroupedEvent) = do
  setStateAff self \s -> s { grouped = not s.grouped }
  eval self UpdateFiles

_ui :: RB.Component Props
_ui = RB.createComponent "UI"

mkUI :: Event ExternalEvent -> Props -> RB.JSX
mkUI externalEvents = RB.make _ui
  { initialState: initialState
  , render: render
  , didMount: \self -> do
      _ <- Event.subscribe externalEvents \ee -> do
        launchAff_ $ eval self (EEQuery ee)
      launchAff_ $ eval self FetchData
  }

data ExternalEvent
  -- move cursor on j/k
  = DirectionEvent Direction
  -- open file on o
  | OpenEvent
  -- mark file watched on m
  | MarkEvent
  -- call refresh files on r
  | RefreshEvent
  -- call fetch icons on I (shift + i)
  | FetchIconsEvent
  -- toggle grouping by show name
  | ToggleGroupedEvent

data Direction = Up | Down

keyboard :: Event Unit -> Effect (Event ExternalEvent)
keyboard _ = do
  {event, push} <- Event.create
  addWindowKeyListener \key ->
    case key of
      "o" -> push OpenEvent
      "k" -> push $ DirectionEvent Up
      "j" -> push $ DirectionEvent Down
      "W" -> push MarkEvent
      "M" -> push MarkEvent
      "r" -> push RefreshEvent
      "I" -> push FetchIconsEvent
      "g" -> push ToggleGroupedEvent
      _ -> pure unit
  pure event

view :: Event ExternalEvent -> Effect (Event Unit)
view externalEvents = do
  renderJSX (mkUI externalEvents {})
  mempty

main :: Effect Unit
main = do
  liftEffect $ runChocoPie mkSink drivers
  Console.log "Started application"
  where
    mkSink sources =
      { keyboard: mempty :: Event Unit
      , view: sources.keyboard
      }
    drivers = {keyboard, view}

setStateAff :: Self -> (State -> State) -> Aff Unit
setStateAff self fn = makeAff \cb -> do
  self.setStateThen fn do cb (Right unit)
  mempty

foreign import renderJSX :: RB.JSX -> Effect Unit

foreign import refreshPage :: Effect Unit

foreign import addWindowKeyListener :: (String -> Effect Unit) -> Effect Unit

foreign import scrollIntoView :: Path -> Effect Unit

foreign import scrollToTop :: Effect Unit

prefixUrl :: String -> M.URL
prefixUrl url = M.URL $ "http://localhost:3000" <> url

get :: forall res url. JSON.ReadForeign res => IsSymbol url => GetRoute res url -> Aff (JSON.E res)
get _ = JSON.read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = M.fetch windowFetch
    action = M.json =<< fetch (prefixUrl url) M.defaultFetchOptions

post :: forall req res url. JSON.WriteForeign req => JSON.ReadForeign res => IsSymbol url =>
  PostRoute req res url -> req -> Aff (JSON.E res)
post _ body = JSON.read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = M.fetch windowFetch
    options =
      { method: M.postMethod
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      , body: JSON.writeJSON body
      }
    action = M.json =<< fetch (prefixUrl url) options
