module FrontEnd where

import Prelude

import CSS as CSS
import ChocoPie (runChocoPie)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe)
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

newtype DateString = DateString String
derive instance newtypeDateString :: Newtype DateString _

type File =
  { name :: Path
  , watched :: Maybe DateString
  , series :: Maybe String
  }

type State =
  -- files
  { files :: Array File
  -- counted by positions from the top
  , cursor :: Maybe Int
  -- TODO: filtering, sorting has turned out to be useless?
  , filterString :: String
  , toggled :: Boolean
  }

-- TODO: add queries i need
data Query a
  = Toggle a
  | FetchData a
  | ToggleWatched Path a
  | SetCursor Int a
  | EEQuery ExternalEvent a

initialState :: State
initialState =
  { files: []
  , cursor: Nothing
  , filterString: ""
  , toggled: false
  }

render :: State -> H.ComponentHTML Query
render state =
  HH.div []
    [ HH.h1_ [HH.text "VT"]
    , header
    , HH.div_ $ Array.mapWithIndex mkFile files
    ]
  where
    label = if state.toggled then "On" else "Off"

    header = HH.div
      [ HP.class_ $ HH.ClassName "header" ]
      [ HH.div [ HP.class_ $ HH.ClassName "filters" ] []
      , HH.div [ HP.class_ $ HH.ClassName "recents" ] $
          [ HH.h3_ [ HH.text "Recently watched:" ]
          ] <> recents
      ]

    recents = mkRecent <$> Array.take 5 files

    mkRecent file = HH.div
      [ HP.class_ $ HH.ClassName "recent" ]
      [ HH.text $ un Path file.name ]

    -- TODO: filtering, maybe sorting
    files = state.files

    mkFile :: Int -> File -> _
    mkFile idx file = HH.div
      [ HP.classes
          [ HH.ClassName "file"
          , HH.ClassName case state.cursor of
              Just pos | pos == idx -> "cursor"
              _ -> ""
          ]
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
      , mkFileCell "name" $ HH.text $ un Path file.name
      , mkFileCell "watched" $ HH.text $ maybe ""
          (\(DateString date) -> "watched " <> date)
          file.watched
      ]

    mkFileCell className e = HH.div
      [HP.class_ $ HH.ClassName className]
      [e]

eval :: Query ~> H.ComponentDSL State Query Void Aff
eval (Toggle next) = do
  state <- H.get
  let nextState = state { toggled = not state.toggled }
  H.put nextState
  pure next

eval (FetchData next) = do
  attempt <- H.liftAff getFiles
  case attempt of
    Right files -> H.modify_ _ { files = files }
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
      , series: hush $ _.name <$> runParser nameParser name
      } :: File

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

eval (EEQuery OpenEvent next) = do
  s <- H.get
  case s.cursor of
    Just pos
      | Just file <- Array.index s.files pos
      -> do
      Console.log $ "Opening file: " <> un Path file.name
      _ <- H.liftAff $ post apiRoutes.open { path: file.name }
      pure next
    _ -> pure next

eval (EEQuery MarkEvent next) = do
  s <- H.get
  case s.cursor of
    Just pos
      | Just file <- Array.index s.files pos
      -> eval (ToggleWatched file.name next)
    _ -> pure next

-- TODO: implement
eval (EEQuery FocusFilterEvent next) = do
  Console.log "FocusFilterEvent"
  pure next

-- TODO: implement
eval (EEQuery RefreshEvent next) = do
  Console.log "RefreshEvent"
  eval (FetchData next)

-- TODO: implement
--     eval (GetIcons next) = do
--       H.modify_ _ {getIcons = Working}
--       result <- post apiRoutes.getIcons {}
--       case result of
--         Right {success} | success -> H.modify_ _ {getIcons = Success}
--         _ -> H.modify_ _ {getIcons = Failure}
--       pure next
eval (EEQuery FetchIconsEvent next) = do
  Console.log "FetchIconsEvent"
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
  -- focus on filter on s
  | FocusFilterEvent
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
      "s" -> push FocusFilterEvent
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

foreign import addWindowKeyListener :: (String -> Effect Unit) -> Effect Unit

-- import Prelude

-- import CSS (backgroundImage, url)
-- import Control.MonadPlus (guard)
-- import Data.Array as Array
-- import Data.Array.NonEmpty as NEA
-- import Data.Either (Either(..), either, hush)
-- import Data.Foldable (maximumBy)
-- import Data.Function (on)
-- import Data.JSDate as JSDate
-- import Data.Map (Map)
-- import Data.Map as Map
-- import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, isNothing, maybe)
-- import Data.Newtype (unwrap, wrap)
-- import Data.Set (Set, insert, member)
-- import Data.String (Pattern(Pattern), contains, toLower)
-- import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
-- import Data.Tuple (Tuple(..))
-- import Effect (Effect)
-- import Effect.Aff (Aff)
-- import Effect.Aff.Class (class MonadAff)
-- import Effect.Class (class MonadEffect)
-- import Effect.Class.Console (error, errorShow, log)
-- import Effect.Unsafe (unsafePerformEffect)
-- import FrontEnd.Chart as Chart
-- import FrontEnd.Style (classNames)
-- import Global.Unsafe (unsafeEncodeURIComponent)
-- import Halogen as H
-- import Halogen.Aff as HA
-- import Halogen.HTML as HH
-- import Halogen.HTML.CSS (style)
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.VDom.Driver as D
-- import Milkis as M
-- import Milkis.Impl.Window (windowFetch)
-- import NameParser (nameParser)
-- import Routes (GetRoute, PostRoute, apiRoutes)
-- import Simple.JSON as JSON
-- import Text.Parsing.StringParser (runParser)
-- import Types (Path(..), WatchedData)

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

-- unE :: forall a m. MonadEffect m => (a -> m Unit) -> JSON.E a -> m Unit
-- unE = either errorShow

-- data Dir = ASC | DSC
-- derive instance eqDir :: Eq Dir
-- data Col = TitleEpisode | Status
-- derive instance eqCol :: Eq Col
-- data Sorting = Sorting Col Dir | NoSorting

-- newtype ChartSeries = ChartSeries (Array ChartSeriesData)
-- newtype ChartSeriesData = ChartSeriesData
--   { date :: String
--   , value :: Int
--   }

-- data RemoteTaskState
--   = Standby
--   | Working
--   | Success
--   | Failure

-- type State =
--   { fileData :: Array FileData
--   , latest ::  Latest
--   , watched :: Array WatchedData
--   , filterWatched :: Boolean
--   , sorting :: Sorting
--   , search :: String
--   , deleteConfirmations :: Set Path
--   , getIcons :: RemoteTaskState
--   }

-- type FileData =
--   { path :: Path
--   , name :: Maybe String
--   , episode :: Maybe String
--   , created :: Maybe String
--   , dateString :: Maybe String
--   , jsDate :: Maybe JSDate.JSDate
--   }

-- type Latest = Map (Maybe String) LatestEntry

-- type LatestEntry =
--   { name :: Maybe String
--   , episode :: Maybe String
--   }

-- getDate :: String -> JSDate.JSDate
-- getDate dateString =
--   -- parsing date is UTZ dependent (ergo effectful), but in our case, we really don't care
--   unsafePerformEffect <<< JSDate.parse $ dateString

-- prepareFilesData :: Array Path -> Array WatchedData -> Array FileData
-- prepareFilesData paths watchedData =
--   go <$> paths
--   where
--     go path@(Path pathString) = do
--       let
--         parsed = hush $ runParser nameParser pathString
--         created = _.created <$> Array.find (\x -> x.path == path) watchedData
--         jsDate = getDate <$> created
--       { path
--       , created
--       , name: _.name <$> parsed
--       , episode: _.episode <$> parsed
--       , dateString: JSDate.toDateString <$> jsDate
--       , jsDate
--       }

-- prepareLatest :: Array FileData -> Array WatchedData -> Latest
-- prepareLatest xs watched =
--   Map.fromFoldable entries
--   where
--     watched' = _.path <$> watched

--     group = Array.groupBy (eq `on` _.name)
--         <<< Array.sortWith _.name
--         <<< Array.filter (\x -> Array.elem x.path watched')

--     entries = mkEntry <$> group xs

--     mkEntry :: NEA.NonEmptyArray FileData -> Tuple (Maybe String) LatestEntry
--     mkEntry ys = do
--       let head = NEA.head ys
--       Tuple
--         head.name
--         { name: head.name
--         , episode: _.episode =<< maximumBy (compare `on` _.episode) ys
--         }

-- getLatest :: Maybe String -> Latest -> String
-- getLatest key latest =
--   fromMaybe "No watched entries found" $ format <$> Map.lookup key latest
--   where
--     format x = "Latest watched of " <> (fromMaybe "" x.name) <> ": " <> (fromMaybe "Unknown episode" x.episode)

-- data Query a
--   = Init a
--   | FetchData a
--   | GetIcons a
--   | OpenFile Path a
--   | SetWatched Path Boolean a
--   | Filter (Maybe String) a
--   | ChangeSorting Col a
--   | Search String a
--   | ClearSearch a
--   | ToggleFilterWatched Boolean a
--   | ConfirmDeletion Path a
--   | Delete Path a

-- type ChildSlots =
--   ( chart :: Chart.Slot Unit
--   )

-- ui :: H.Component HH.HTML Query Unit Void Aff
-- ui =
--   H.component
--     { initialState: const initialState
--     , render
--     , eval
--     , receiver: const Nothing
--     , initializer: Just (H.action Init)
--     , finalizer: Nothing
--     }
--   where
--     initialState :: State
--     initialState =
--       { fileData: mempty
--       , latest: Map.empty
--       , watched: mempty
--       , filterWatched: false
--       , sorting: NoSorting
--       , search: mempty
--       , deleteConfirmations: mempty
--       , getIcons: Standby
--       }

--     render :: State -> H.ComponentHTML Query ChildSlots Aff
--     render state =
--       HH.div
--         [ HP.class_ $ classNames.container ]
--         $ [ HH.h1_ [ HH.text "Vidtracker" ]
--           , heatmap
--           , HH.div
--               [ HP.class_ classNames.top ]
--               [ HH.div [ HP.class_ classNames.topLeft ]
--                   [ refreshButton
--                   , getIconsButton
--                   , filterCheckbox
--                   , search
--                   ]
--               , HH.div [ HP.class_ classNames.topRight ] recentHistory
--               ]
--           , header
--           ] <> files
--       where
--         recentHistory = do
--           let
--             recents
--               = Array.take 5 <<< Array.reverse <<< Array.sortWith _.jsDate
--               $ Array.filter (isJust <<< _.jsDate) state.fileData
--             list = recents <#> \x -> HH.div_ [ HH.text (unwrap x.path) ]
--           [ HH.h4_ [ HH.text "Recently watched:" ] ] <> list
--         heatmap =
--           HH.slot (SProxy :: SProxy "chart") unit Chart.component state.watched absurd
--         refreshButton =
--           HH.button
--             [ HP.classes $
--               [ classNames.refreshFiles
--               , wrap "pure-button"
--               ]
--             , HE.onClick <<< HE.input_ $ FetchData
--             ]
--             [ HH.text "Refresh" ]
--         getIconsButton =
--           HH.button
--             [ HP.classes $
--               [ classNames.getIcons
--               , wrap "pure-button"
--               ]
--             , HE.onClick <<< HE.input_ $ GetIcons
--             ]
--             [ HH.text
--                $ "Run Get Icons"
--               <> case state.getIcons of
--                    Standby -> ": Standby"
--                    Working -> ": Working..."
--                    Success -> ": Success"
--                    Failure -> ": Failed"
--             ]
--         filterCheckbox =
--           HH.button
--             [ HP.classes $
--               [ classNames.filterWatched
--               , wrap "pure-button"
--               ] <> (guard state.filterWatched $> wrap "pure-button-primary")
--             , HE.onClick <<< HE.input_ <<< ToggleFilterWatched <<< not $ state.filterWatched
--             ]
--             [ HH.text "Filter Watched" ]
--         search =
--           HH.div_
--             $ [ HH.h4_ [ HH.text "Search" ]
--               , HH.input
--                   [ HP.value state.search
--                   , HE.onValueInput (HE.input Search)
--                   ]
--               ] <> clear
--         clear =
--           guard (state.search /= "") $>
--             HH.button
--               [ HE.onClick $ HE.input_ ClearSearch ]
--               [ HH.text "Clear" ]
--         header =
--           HH.div
--             [ HP.class_ $ classNames.file]
--             [ HH.h3
--               [ HP.class_ $ classNames.dot
--               ] []
--             , HH.h3
--               [ HP.class_ $ classNames.fileLink
--               , HE.onClick $ HE.input_ (ChangeSorting TitleEpisode)
--               ] [ HH.text $ "Title" <> displayTicker TitleEpisode ]
--             , HH.h3
--               [ HP.class_ $ classNames.fileEpisode
--               , HE.onClick $ HE.input_ (ChangeSorting TitleEpisode)
--               ] []
--             , HH.h3
--               [ HP.class_ $ classNames.fileButton
--               , HE.onClick $ HE.input_ (ChangeSorting Status)
--               ] [ HH.text $ "Status" <> displayTicker Status ]
--             , HH.h3 [HP.class_ $ classNames.fileNote ] [ HH.text "Date" ]
--             , HH.h3
--               [ HP.class_ $ classNames.filterLink
--               ] [ HH.text "" ]
--             , HH.h3
--               [ HP.class_ $ classNames.deleteLink
--               ] [ HH.text "" ]
--             ]
--         files = file <$> applyTransforms state.fileData

--         displayTicker col
--           | Sorting col' dir <- state.sorting
--           , asc <- dir == ASC
--           , col' == col = if asc
--             then " ASC"
--             else " DSC"
--           | otherwise = ""
--         applyTransforms
--             = applySorting
--           <<< applySearchFiltering
--           <<< applyWatchFiltering
--         applyWatchFiltering = if state.filterWatched
--           then Array.filter $ isNothing <<< _.created
--           else identity
--         applySearchFiltering = case state.search of
--             "" -> identity
--             x -> Array.filter $ \{ path: Path path } -> contains (Pattern $ toLower x) (toLower path)
--         applySorting
--           | Sorting col dir <- state.sorting
--           , rev <- if dir == ASC
--             then identity
--             else Array.reverse
--           , sort' <- case col of
--             TitleEpisode -> \xs -> do
--               let
--                 preGroup = Array.sortWith _.name xs
--                 grouped = Array.groupBy (\{name: nameA} {name: nameB} -> nameA == nameB) preGroup
--                 sorted = Array.sortWith _.episode <<< Array.fromFoldable <$> grouped
--               join sorted
--             Status -> Array.sortWith _.created
--             _ -> identity
--           = rev <<< sort'
--           | otherwise = identity
--         parseEpisodeNumber { path: Path path } = case runParser nameParser path of
--           Right {episode} -> episode
--           Left _ -> "999"
--         file { path, created, name, episode, dateString } =
--           HH.div
--             [ HP.class_ $ classNames.file ]
--             [ HH.span
--               [ HP.class_ $ classNames.dot
--               , style do
--                 case name of
--                   Just s ->
--                     backgroundImage (url $ "icons/" <> unsafeEncodeURIComponent s)
--                   Nothing -> pure mempty
--               ] []
--             , HH.a
--               [ HP.class_ classNames.fileLink
--               , HP.title $ getLatest name state.latest
--               , HE.onClick $ HE.input_ (OpenFile path) ]
--               [ HH.text $ unwrap path ]
--             , HH.span
--                 [ HP.class_ $ classNames.fileEpisode ]
--                 [ HH.text $ fromMaybe "" episode ]
--             , HH.button
--               [ HP.classes $
--                 [ classNames.fileButton
--                 , wrap "pure-button"
--                 , wrap $ maybe "" (const "pure-button-primary") dateString
--                 ]
--               , HE.onClick $ HE.input_ (SetWatched path (not $ isJust dateString))
--               ]
--               [ HH.text $ maybe "not watched" (const "watched") dateString ]
--             , HH.span
--               [ HP.class_ $ classNames.fileNote ]
--               [ HH.text $ fromMaybe "" dateString ]
--             , HH.button
--               [ HP.classes $
--                 [ classNames.filterLink
--                 , wrap "pure-button"
--                 ]
--               , HE.onClick $ HE.input_ (Filter name)
--               ]
--               [ HH.text "set filter" ]
--             , HH.button
--               [ HP.classes $
--                 [ classNames.deleteLink
--                 , wrap "pure-button"
--                 , if deleteConfirmation
--                     then classNames.deleteConfirmation
--                     else wrap ""
--                 ]
--               , HE.onClick $ HE.input_ $
--                   if deleteConfirmation
--                     then (Delete path)
--                     else (ConfirmDeletion path)
--               ]
--               [ HH.text
--                   if deleteConfirmation
--                     then "Confirm"
--                     else "Delete"
--               ]
--             ]
--           where
--             deleteConfirmation = member path state.deleteConfirmations

--     eval :: Query ~> H.HalogenM State Query ChildSlots Void Aff
--     eval (Init next) = do
--       eval (FetchData next)





--     eval (Filter name next) = do
--       case name of
--         Nothing -> error "Can't filter by unknown name" *> pure next
--         Just s -> eval $ Search s next

--     eval (Search str next) = do
--       H.modify_ _ {search = str}
--       pure next

--     eval (ClearSearch next) = do
--       H.modify_ _ {search = ""}
--       pure next

--     eval (ChangeSorting col next)= do
--       H.modify_ $ \s -> case s.sorting of
--         Sorting curr dir | curr == col ->
--           s {sorting = case dir of
--               ASC -> Sorting col DSC
--               DSC -> NoSorting
--             }
--         _ -> s {sorting = Sorting col ASC}
--       pure next

--     eval (ToggleFilterWatched flag next) = do
--       H.modify_ _ {filterWatched = flag}
--       pure next

--     eval (ConfirmDeletion path next) = do
--       H.modify_ \s -> s {deleteConfirmations = insert path s.deleteConfirmations}
--       pure next

--     eval (Delete path next) = do
--       _ <- post apiRoutes.remove {path}
--       eval (FetchData next)

-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   body <- HA.awaitBody
--   io <- D.runUI ui unit body

--   log $ "Running"
