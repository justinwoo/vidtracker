module FrontEnd where

import Prelude

import CSS (backgroundImage, url)
import Control.MonadPlus (guard)
import Data.Array (filter, reverse, sort, sortWith)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(Nothing, Just), isJust, isNothing, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set, insert, member)
import Data.String (Pattern(Pattern), contains, toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error, errorShow, log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (MultipleErrors)
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
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Text.Parsing.StringParser (runParser)
import Types (Path(..), WatchedData)

extractNameKinda :: Path -> Either String String
extractNameKinda (Path s) =
  bimap show _.name $ runParser nameParser s

type E a = Either MultipleErrors a

get
  :: forall res url m
   . MonadAff m
  => ReadForeign res
  => IsSymbol url
  => GetRoute res url -> m (E res)
get _ =
  H.liftAff $ read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = M.fetch windowFetch
    action = M.json =<< fetch (M.URL url) M.defaultFetchOptions

post
  :: forall req res url m
   . MonadAff m
  => WriteForeign req
  => ReadForeign res
  => IsSymbol url
  => PostRoute req res url -> req -> m (E res)
post _ body =
  H.liftAff $ read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = M.fetch windowFetch
    options =
      { method: M.postMethod
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      , body: writeJSON body
      }
    action = M.json =<< fetch (M.URL url) options

unE :: forall a m. MonadEffect m => (a -> m Unit) -> E a -> m Unit
unE = either errorShow

data Dir = ASC | DSC
derive instance eqDir :: Eq Dir
data Col = Title | Status | Episode
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
  { files :: Array Path
  , watched :: Array WatchedData
  , filterWatched :: Boolean
  , sorting :: Sorting
  , search :: String
  , deleteConfirmations :: Set Path
  , getIcons :: RemoteTaskState
  }

data Query a
  = Init a
  | FetchData a
  | GetIcons a
  | OpenFile Path a
  | SetWatched Path Boolean a
  | Filter Path a
  | ChangeSorting Col a
  | Search String a
  | ClearSearch a
  | ToggleFilterWatched Boolean a
  | ConfirmDeletion Path a
  | Delete Path a

data Slot = ChartSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.lifecycleParentComponent
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
      , filterWatched: false
      , sorting: NoSorting
      , search: mempty
      , deleteConfirmations: mempty
      , getIcons: Standby
      }

    render :: State -> H.ParentHTML Query Chart.Query Slot Aff
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
          HH.slot ChartSlot Chart.component state.watched absurd
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
              , HE.onClick $ HE.input_ (ChangeSorting Title)
              ] [ HH.text $ "Title" <> displayTicker Title ]
            , HH.h3
              [ HP.class_ $ classNames.fileEpisode
              , HE.onClick $ HE.input_ (ChangeSorting Episode)
              ] [ HH.text $ "No" <> displayTicker Episode ]
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
        files =
          file <$> applyTransforms state.files

        displayTicker col
          | Sorting col' dir <- state.sorting
          , asc <- dir == ASC
          , col' == col = if asc
            then " ASC"
            else " DSC"
          | otherwise = ""
        applyTransforms = applySorting <<< applySearchFiltering <<< applyWatchFiltering
        applyWatchFiltering = if state.filterWatched
          then filter $ isNothing <<< findWatched
          else identity
        applySearchFiltering = case state.search of
            "" -> identity
            x -> filter $ \(Path path) -> contains (Pattern $ toLower x) (toLower path)
        applySorting
          | Sorting col dir <- state.sorting
          , rev <- if dir == ASC
            then identity
            else reverse
          , sort' <- case col of
            Title -> sort
            Episode -> sortWith parseEpisodeNumber
            Status -> sortWith (map _.created <<< findWatched)
            _ -> identity
          = rev <<< sort'
          | otherwise = identity
        parseEpisodeNumber (Path path) = case runParser nameParser path of
          Right {episode} -> episode
          Left _ -> "999"
        findWatched path = find (\fd -> fd.path == path) state.watched
        file path =
          HH.div
            [ HP.class_ $ classNames.file ]
            [ HH.span
              [ HP.class_ $ classNames.dot
              , style do
                case extractNameKinda path of
                  Right name ->
                    backgroundImage (url $ "icons/" <> unsafeEncodeURIComponent name)
                  Left e -> pure mempty
              ] []
            , HH.a
              [ HP.class_ classNames.fileLink
              , HE.onClick $ HE.input_ (OpenFile path) ]
              [ HH.text $ unwrap path ]
            , HH.span
                [ HP.class_ $ classNames.fileEpisode ]
                [ HH.text $ either (const "") _.episode $ runParser nameParser (unwrap path) ]
            , HH.button
              [ HP.classes $
                [ classNames.fileButton
                , wrap "pure-button"
                , wrap $ maybe "" (const "pure-button-primary") watched
                ]
              , HE.onClick $ HE.input_ (SetWatched path (not $ isJust watched))
              ]
              [ HH.text $ maybe "not watched" (const "watched") watched ]
            , HH.span
              [ HP.class_ $ classNames.fileNote ]
              [ HH.text $ maybe "" identity watched ]
            , HH.button
              [ HP.classes $
                [ classNames.filterLink
                , wrap "pure-button"
                ]
              , HE.onClick $ HE.input_ (Filter path)
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
            watched = getDate <$> findWatched path
            deleteConfirmation = member path state.deleteConfirmations
            getDate {created} =
              -- parsing date is UTZ dependent (ergo effectful), but in our case, we really don't care
              JSDate.toDateString <<< unsafePerformEffect <<< JSDate.parse $ created

    eval :: Query ~> H.ParentDSL State Query Chart.Query Slot Void Aff
    eval (Init next) = do
      eval (FetchData next)

    eval (FetchData next) = do
      getResult >>= unE
        \(Tuple f w) -> do
          H.modify_ _ {files = f, watched = w}
      pure next
      where
        getResult = do
          files <- get apiRoutes.files
          watched <- get apiRoutes.watched
          pure $ Tuple <$> files <*> watched

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

    eval (Filter path next) = do
      case extractNameKinda path of
        Left e -> error e *> pure next
        Right s -> eval $ Search s next

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
