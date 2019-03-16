module FrontEnd where

import Prelude

import CSS as CSS
import ChocoPie (runChocoPie)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import Global.Unsafe (unsafeEncodeURIComponent)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import NameParser (nameParser)
import Routes (GetRoute, PostRoute, apiRoutes)
import Simple.JSON as JSON
import Text.Parsing.StringParser (runParser)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Types (Path(..))
import Web.Event.Event as E
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

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
  , filesLoading :: Boolean
  , iconsLoading :: Boolean
  -- counted by positions from the top
  , cursor :: Maybe Int
  }

data Query a
  = FetchData a
  | OpenFile a
  | MarkFile a
  | ToggleWatched Path a
  | SetCursor Int a
  | LinkClick Int MouseEvent a
  | WatchedClick Int MouseEvent a
  | EEQuery ExternalEvent a

initialState :: State
initialState =
  { files: []
  , filesLoading: false
  , iconsLoading: false
  , cursor: Nothing
  }

render :: State -> H.ComponentHTML Query
render state =
  HH.div []
    [ HH.h1_ [HH.text "vidtracker"]
    , header
    , HH.div_ $ Array.mapWithIndex mkFile state.files
    ]
  where
    header = HH.div
      [ HP.class_ $ HH.ClassName "header" ]
      [ HH.div [ HP.class_ $ HH.ClassName "info" ] $
          [ HH.h3_ [ HH.text "Info:" ] ] <> infoLines
      , HH.div [ HP.class_ $ HH.ClassName "recents" ] $
          [ HH.h3_ [ HH.text "Recently watched:" ] ] <> recents
      ]

    infoLines = HH.span_ <<< pure <<< HH.text <$>
      [ "o: open current file"
      , "k: move cursor up"
      , "j: move cursor down"
      , "W/M: mark as watched"
      , "r: refresh"
      , "I: fetch icons and reload page"
      , "files loading: " <> if state.filesLoading then "true" else "false"
      , "icons loading: " <> if state.iconsLoading then "true" else "false"
      ]

    recents = mkRecent <$> Array.take (Array.length infoLines) state.files

    mkRecent file = mkDiv "recent" $ HH.text $ un Path file.name

    mkFile :: Int -> File -> _
    mkFile idx file = HH.div
      [ HP.classes
          [ HH.ClassName "file"
          , HH.ClassName case state.cursor of
              Just pos | pos == idx -> "cursor"
              _ -> ""
          , HH.ClassName case file.watched of
              Just _ -> "done"
              Nothing -> ""
          ]
      , HP.title $ "latest watched: " <> fromMaybe "unknown" (show <$> file.latest)
      , HE.onClick $ HE.input_ $ SetCursor idx
      ]
      [ HH.div
          [ HP.class_ $ HH.ClassName "icon"
          , HCSS.style do
              case file.series of
                Just series ->
                  CSS.backgroundImage (CSS.url $ un M.URL $ prefixUrl $ "/icons/" <> unsafeEncodeURIComponent series)
                Nothing ->
                  pure mempty
          ] []
      , HH.div
          [ HP.class_ $ HH.ClassName "name"
          , HE.onClick $ HE.input $ LinkClick idx
          ]
          [ HH.text $ un Path file.name ]
      , HH.div
          [ HP.classes $ HH.ClassName <$>
            [ "watched"
            , maybe "" (const "has-date") file.watched
            ]
          , HE.onClick $ HE.input $ WatchedClick idx
          ]
          [ HH.text $ case file.watched of
              Just (DateString date) -> "watched " <> date
              Nothing -> maybe "" (\x -> " last: " <> show x) file.latest
          ]
      ]

    mkDiv className e = HH.div
      [HP.class_ $ HH.ClassName className]
      [e]

eval :: Query ~> H.ComponentDSL State Query Void Aff

eval (LinkClick idx e next) = do
  H.liftEffect $ E.preventDefault $ ME.toEvent e
  _ <- eval (SetCursor idx next)
  eval (OpenFile next)

eval (WatchedClick idx e next) = do
  H.liftEffect $ E.preventDefault $ ME.toEvent e
  _ <- eval (SetCursor idx next)
  eval (MarkFile next)

eval (FetchData next) = do
  H.modify_ _ { filesLoading = true }
  attempt <- H.liftAff $ getFiles
  case attempt of
    Right files -> H.modify_ _ { files = annotateLatest files, filesLoading = false }
    Left e -> Console.error $ "Failed to fetch data: " <> show e
  pure next
  where
    getFiles = ado
      files <- get apiRoutes.files
      watched <- get apiRoutes.watched
      in (\fs w -> mkFile w <$> fs) <$> files <*> watched

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
            = maximumBy (compare `on` _.series)
          <$> Array.groupBy (eq `on` _.series) watchedFiles
        updateLatest x
          | match' <- \z -> z.series == x.series
          , match <- \y -> maybe false match' y
          , Just (Just y) <- Array.find match grouped
            = x { latest = y.episode }
          | otherwise = x

eval (ToggleWatched name next) = do
  Console.log $ "Updating " <> un Path name
  s <- H.get
  case Array.find (\x -> x.name == name) s.files of
    Just file -> do
      _ <- H.liftAff $ post apiRoutes.update
        { path: file.name
        , watched: maybe true (const false) file.watched
        }
      pure unit
    Nothing -> Console.error $ "Could not find file named " <> un Path name
  eval (FetchData next)

eval (SetCursor idx next) = do
  H.modify_ _ { cursor = Just idx }
  pure next

eval (EEQuery (DirectionEvent dir) next) = do
  case dir of
    Down -> H.modify_ \s -> do
      let cursor = min (Array.length s.files) (maybe 0 (_ + 1) s.cursor)
      s { cursor = Just cursor }
    Up -> H.modify_ \s -> do
      let cursor = max 0 (maybe 0 (_ - 1) s.cursor)
      s { cursor = Just cursor }
  pure next

eval (OpenFile next) = do
  s <- H.get
  case s.cursor of
    Just pos
      | Just file <- Array.index s.files pos
      -> do
      Console.log $ "Opening file: " <> un Path file.name
      _ <- H.liftAff $ post apiRoutes.open { path: file.name }
      pure next
    _ -> pure next

eval (EEQuery OpenEvent next) = eval (OpenFile next)

eval (MarkFile next) = do
  s <- H.get
  case s.cursor of
    Just pos
      | Just file <- Array.index s.files pos
      -> eval (ToggleWatched file.name next)
    _ -> pure next

eval (EEQuery MarkEvent next) = eval (MarkFile next)

eval (EEQuery RefreshEvent next) = do
  Console.log "RefreshEvent"
  eval (FetchData next)

eval (EEQuery FetchIconsEvent next) = do
  Console.log "FetchIconsEvent"
  H.modify_ _ { iconsLoading = true }
  result <- H.liftAff $ post apiRoutes.getIcons {}
  H.modify_ _ { iconsLoading = false }
  H.liftEffect $ refreshPage
  pure next

myButton :: H.Component HH.HTML Query Unit Void Aff
myButton =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action FetchData)
    , finalizer: Nothing
    , receiver: const Nothing
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
      _ -> pure unit
  pure event

halogen :: Event ExternalEvent -> Effect (Event Unit)
halogen externalEvents = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI myButton unit body
    _ <- liftEffect $ Event.subscribe externalEvents \ee -> do
      launchAff_ $ io.query $ H.action $ EEQuery ee
      pure $ mempty :: Event Unit
    pure unit
  mempty

main :: Effect Unit
main = do
  liftEffect $ runChocoPie mkSink drivers
  Console.log "Started application"
  where
    mkSink sources =
      { keyboard: mempty :: Event Unit
      , halogen: sources.keyboard
      }
    drivers = {keyboard, halogen}

foreign import refreshPage :: Effect Unit

foreign import addWindowKeyListener :: (String -> Effect Unit) -> Effect Unit

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
