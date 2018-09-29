module FrontEnd where

import Prelude

import CSS (backgroundImage, url)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set, insert, member)
import Data.String (Pattern(Pattern), contains, toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error, errorShow, log)
import Effect.Unsafe (unsafePerformEffect)
import FrontEnd.Chart as Chart
import FrontEnd.Style (classNames)
import Global.Unsafe (unsafeEncodeURIComponent)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import NameParser (nameParser)
import Routes (GetRoute, PostRoute, apiRoutes)
import Simple.JSON as JSON
import Text.Parsing.StringParser (runParser)
import Types (Path(..), WatchedData)

get
  :: forall res url m
   . MonadAff m
  => JSON.ReadForeign res
  => IsSymbol url
  => GetRoute res url -> m (JSON.E res)
get _ =
  H.liftAff $ JSON.read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = M.fetch windowFetch
    action = M.json =<< fetch (M.URL url) M.defaultFetchOptions

post
  :: forall req res url m
   . MonadAff m
  => JSON.WriteForeign req
  => JSON.ReadForeign res
  => IsSymbol url
  => PostRoute req res url -> req -> m (JSON.E res)
post _ body =
  H.liftAff $ JSON.read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = M.fetch windowFetch
    options =
      { method: M.postMethod
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      , body: JSON.writeJSON body
      }
    action = M.json =<< fetch (M.URL url) options

unE :: forall a m. MonadEffect m => (a -> m Unit) -> JSON.E a -> m Unit
unE = either errorShow

data Dir = ASC | DSC
derive instance eqDir :: Eq Dir
data Col = TitleEpisode | Status
derive instance eqCol :: Eq Col
data Sorting = Sorting Col Dir | NoSorting

newtype ChartSeries = ChartSeries (Array ChartSeriesData)
newtype ChartSeriesData = ChartSeriesData
  { date :: String
  , value :: Int
  }

data RemoteTaskState
  = Standby
  | Working
  | Success
  | Failure

type State =
  { fileData :: Array FileData
  , watched :: Array WatchedData
  , filterWatched :: Boolean
  , sorting :: Sorting
  , search :: String
  , deleteConfirmations :: Set Path
  , getIcons :: RemoteTaskState
  }

type FileData =
  { path :: Path
  , name :: Maybe String
  , episode :: Maybe String
  , created :: Maybe String
  }

prepareFilesData :: Array Path -> Array WatchedData -> Array FileData
prepareFilesData paths watchedData =
  go <$> paths
  where
    go path@(Path pathString) = do
      let parsed = hush $ runParser nameParser pathString
      { path
      , created: _.created <$> Array.find (\x -> x.path == path) watchedData
      , name: _.name <$> parsed
      , episode: _.episode <$> parsed
      }

data Query a
  = Init a
  | FetchData a
  | GetIcons a
  | OpenFile Path a
  | SetWatched Path Boolean a
  | Filter (Maybe String) a
  | ChangeSorting Col a
  | Search String a
  | ClearSearch a
  | ToggleFilterWatched Boolean a
  | ConfirmDeletion Path a
  | Delete Path a

type ChildSlots =
  ( chart :: Chart.Slot Unit
  )

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
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
      { fileData: mempty
      , watched: mempty
      , filterWatched: false
      , sorting: NoSorting
      , search: mempty
      , deleteConfirmations: mempty
      , getIcons: Standby
      }

    render :: State -> H.ComponentHTML Query ChildSlots Aff
    render state =
      HH.div
        [ HP.class_ $ classNames.container ]
        $ [ HH.h1_ [ HH.text "Vidtracker" ]
          , heatmap
          , refreshButton
          , getIconsButton
          , filterCheckbox
          , search
          , header
          ] <> files
      where
        heatmap =
          HH.slot (SProxy :: SProxy "chart") unit Chart.component state.watched absurd
        refreshButton =
          HH.button
            [ HP.classes $
              [ classNames.refreshFiles
              , wrap "pure-button"
              ]
            , HE.onClick <<< HE.input_ $ FetchData
            ]
            [ HH.text "Refresh" ]
        getIconsButton =
          HH.button
            [ HP.classes $
              [ classNames.getIcons
              , wrap "pure-button"
              ]
            , HE.onClick <<< HE.input_ $ GetIcons
            ]
            [ HH.text
               $ "Run Get Icons"
              <> case state.getIcons of
                   Standby -> ": Standby"
                   Working -> ": Working..."
                   Success -> ": Success"
                   Failure -> ": Failed"
            ]
        filterCheckbox =
          HH.button
            [ HP.classes $
              [ classNames.filterWatched
              , wrap "pure-button"
              ] <> (guard state.filterWatched $> wrap "pure-button-primary")
            , HE.onClick <<< HE.input_ <<< ToggleFilterWatched <<< not $ state.filterWatched
            ]
            [ HH.text "Filter Watched" ]
        search =
          HH.div_
            $ [ HH.h4_ [ HH.text "Search" ]
              , HH.input
                  [ HP.value state.search
                  , HE.onValueInput (HE.input Search)
                  ]
              ] <> clear
        clear =
          guard (state.search /= "") $>
            HH.button
              [ HE.onClick $ HE.input_ ClearSearch ]
              [ HH.text "Clear" ]
        header =
          HH.div
            [ HP.class_ $ classNames.file]
            [ HH.h3
              [ HP.class_ $ classNames.dot
              ] []
            , HH.h3
              [ HP.class_ $ classNames.fileLink
              , HE.onClick $ HE.input_ (ChangeSorting TitleEpisode)
              ] [ HH.text $ "Title" <> displayTicker TitleEpisode ]
            , HH.h3
              [ HP.class_ $ classNames.fileEpisode
              , HE.onClick $ HE.input_ (ChangeSorting TitleEpisode)
              ] []
            , HH.h3
              [ HP.class_ $ classNames.fileButton
              , HE.onClick $ HE.input_ (ChangeSorting Status)
              ] [ HH.text $ "Status" <> displayTicker Status ]
            , HH.h3 [HP.class_ $ classNames.fileNote ] [ HH.text "Date" ]
            , HH.h3
              [ HP.class_ $ classNames.filterLink
              ] [ HH.text "" ]
            , HH.h3
              [ HP.class_ $ classNames.deleteLink
              ] [ HH.text "" ]
            ]
        files = file <$> applyTransforms state.fileData

        displayTicker col
          | Sorting col' dir <- state.sorting
          , asc <- dir == ASC
          , col' == col = if asc
            then " ASC"
            else " DSC"
          | otherwise = ""
        applyTransforms
            = applySorting
          <<< applySearchFiltering
          <<< applyWatchFiltering
        applyWatchFiltering = if state.filterWatched
          then Array.filter $ isNothing <<< _.created
          else identity
        applySearchFiltering = case state.search of
            "" -> identity
            x -> Array.filter $ \{ path: Path path } -> contains (Pattern $ toLower x) (toLower path)
        applySorting
          | Sorting col dir <- state.sorting
          , rev <- if dir == ASC
            then identity
            else Array.reverse
          , sort' <- case col of
            TitleEpisode -> \xs -> do
              let
                grouped = Array.groupBy (\{name: nameA} {name: nameB} -> nameA == nameB) xs
                sorted = Array.sortWith _.episode <<< Array.fromFoldable <$> grouped
              join sorted
            Status -> Array.sortWith _.created
            _ -> identity
          = rev <<< sort'
          | otherwise = identity
        parseEpisodeNumber { path: Path path } = case runParser nameParser path of
          Right {episode} -> episode
          Left _ -> "999"
        file { path, created, name, episode } =
          HH.div
            [ HP.class_ $ classNames.file ]
            [ HH.span
              [ HP.class_ $ classNames.dot
              , style do
                case name of
                  Just s ->
                    backgroundImage (url $ "icons/" <> unsafeEncodeURIComponent s)
                  Nothing -> pure mempty
              ] []
            , HH.a
              [ HP.class_ classNames.fileLink
              , HE.onClick $ HE.input_ (OpenFile path) ]
              [ HH.text $ unwrap path ]
            , HH.span
                [ HP.class_ $ classNames.fileEpisode ]
                [ HH.text $ fromMaybe "" episode ]
            , HH.button
              [ HP.classes $
                [ classNames.fileButton
                , wrap "pure-button"
                , wrap $ maybe "" (const "pure-button-primary") watchedDate
                ]
              , HE.onClick $ HE.input_ (SetWatched path (not $ isJust watchedDate))
              ]
              [ HH.text $ maybe "not watched" (const "watched") watchedDate ]
            , HH.span
              [ HP.class_ $ classNames.fileNote ]
              [ HH.text $ maybe "" identity watchedDate ]
            , HH.button
              [ HP.classes $
                [ classNames.filterLink
                , wrap "pure-button"
                ]
              , HE.onClick $ HE.input_ (Filter name)
              ]
              [ HH.text "set filter" ]
            , HH.button
              [ HP.classes $
                [ classNames.deleteLink
                , wrap "pure-button"
                , if deleteConfirmation
                    then classNames.deleteConfirmation
                    else wrap ""
                ]
              , HE.onClick $ HE.input_ $
                  if deleteConfirmation
                    then (Delete path)
                    else (ConfirmDeletion path)
              ]
              [ HH.text
                  if deleteConfirmation
                    then "Confirm"
                    else "Delete"
              ]
            ]
          where
            watchedDate = getDate <$> created
            deleteConfirmation = member path state.deleteConfirmations
            getDate dateString =
              -- parsing date is UTZ dependent (ergo effectful), but in our case, we really don't care
              JSDate.toDateString <<< unsafePerformEffect <<< JSDate.parse $ dateString

    eval :: Query ~> H.HalogenM State Query ChildSlots Void Aff
    eval (Init next) = do
      eval (FetchData next)

    eval (FetchData next) = do
      getResult >>= unE
        \{filesData: fd, watched: w} -> do
          H.modify_ _ {fileData = fd, watched = w}
      pure next
      where
        getResult = do
          files <- get apiRoutes.files
          watched <- get apiRoutes.watched
          let filesData = prepareFilesData <$> files <*> watched
          pure $ { filesData: _, watched: _ } <$> filesData <*> watched

    eval (GetIcons next) = do
      H.modify_ _ {getIcons = Working}
      result <- post apiRoutes.getIcons {}
      case result of
        Right {success} | success -> H.modify_ _ {getIcons = Success}
        _ -> H.modify_ _ {getIcons = Failure}
      pure next

    eval (OpenFile path next) = do
      _ <- post apiRoutes.open {path}
      pure next

    eval (SetWatched path flag next) = do
      post apiRoutes.update {path, watched: flag}
        >>= unE \w -> H.modify_ _ {watched = w}
      pure next

    eval (Filter name next) = do
      case name of
        Nothing -> error "Can't filter by unknown name" *> pure next
        Just s -> eval $ Search s next

    eval (Search str next) = do
      H.modify_ _ {search = str}
      pure next

    eval (ClearSearch next) = do
      H.modify_ _ {search = ""}
      pure next

    eval (ChangeSorting col next)= do
      H.modify_ $ \s -> case s.sorting of
        Sorting curr dir | curr == col ->
          s {sorting = case dir of
              ASC -> Sorting col DSC
              DSC -> NoSorting
            }
        _ -> s {sorting = Sorting col ASC}
      pure next

    eval (ToggleFilterWatched flag next) = do
      H.modify_ _ {filterWatched = flag}
      pure next

    eval (ConfirmDeletion path next) = do
      H.modify_ \s -> s {deleteConfirmations = insert path s.deleteConfirmations}
      pure next

    eval (Delete path next) = do
      _ <- post apiRoutes.remove {path}
      eval (FetchData next)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  log $ "Running"
