module FrontEnd where

import Prelude
import Data.JSDate as JSDate
import ECharts.Chart as EC
import ECharts.Commands as E
import ECharts.Monad as EM
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
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
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.HTML.Types (readHTMLElement)
import Data.Array (filter, group', intercalate, length, reverse, sort, sortWith)
import Data.Date (Date)
import Data.DateTime (adjust, date)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Encode, class Decode, encode)
import Data.Foreign.Generic (decodeJSON)
import Data.Int (ceil, toNumber)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Nothing, Just), isJust, isNothing, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty, head, oneOf)
import Data.Number.Format (toString)
import Data.String (Pattern(..), contains, split, toLower)
import Data.Time.Duration (Days(..))
import Data.Traversable (find, traverse_)
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

newtype ChartSeries = ChartSeries (Array ChartSeriesData)
newtype ChartSeriesData = ChartSeriesData
  { date :: String
  , value :: Int
  }

type State =
  { files :: Array Path
  , watched :: Array WatchedData
  , filterWatched :: Boolean
  , sorting :: Sorting
  , search :: String
  , chart :: Maybe ET.Chart
  }

data Query a
  = Init a
  | FetchData a
  | OpenFile Path a
  | SetWatched Path Boolean a
  | Filter Path a
  | ChangeSorting Col a
  | Search String a
  | ClearSearch a
  | ToggleFilterWatched Boolean a

type AppEffects eff =
  Aff
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM
  , echarts :: ET.ECHARTS
  , exception :: EXCEPTION
  , now :: NOW
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
      , filterWatched: false
      , sorting: NoSorting
      , search: mempty
      , chart: Nothing
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
        $ [ HH.h1_ [ HH.text "Vidtracker" ]
          , heatmap
          , refreshButton
          , filterCheckbox
          , search
          , header
          ] <> files
      where
        heatmap =
          HH.div
            [ HP.ref $ wrap "heatmap"
            , HP.class_ $ wrap "heatmap"]
            []
        refreshButton =
          HH.button
            [ HP.classes $ wrap <$>
              [ "refresh-files"
              , "pure-button"
              ]
            , HE.onClick <<< HE.input_ $ FetchData
            ]
            [ HH.text "Refresh" ]
        filterCheckbox =
          HH.button
            [ HP.classes $ wrap <$>
              [ "filter-watched"
              , "pure-button"
              ] <> (guard state.filterWatched $> "pure-button-primary")
            , HE.onClick <<< HE.input_ <<< ToggleFilterWatched <<< not $ state.filterWatched
            ]
            [ HH.text "Filter Watched" ]
        search =
          HH.div
            [ HP.class_ $ wrap "search" ]
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
            , HH.h3
              [ HP.class_ $ wrap "filter-link"
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
          else id
        applySearchFiltering = case state.search of
            "" -> id
            x -> filter $ \(Path path) -> contains (Pattern $ toLower x) (toLower path)
        applySorting
          | Sorting col dir <- state.sorting
          , rev <- if dir == ASC
            then id
            else reverse
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
            , HH.button
              [ HP.classes $ wrap <$>
                [ "filter-link"
                , "pure-button"
                ]
              , HE.onClick $ HE.input_ (Filter path)
              ]
              [ HH.text "set filter" ]
            ]
          where
            watched = getDate <$> findWatched path
            getDate (WatchedData {created}) =
              -- parsing date is UTZ dependent (ergo effectful), but in our case, we really don't care
              JSDate.toDateString <<< unsafePerformEff <<< JSDate.parse $ created

    error' = H.liftEff <<< error

    updateChart' w = do
      chart <- H.gets _.chart
      case chart of
        Just ch -> do
          now <- toDateTime <$> H.liftEff now
          case date <$> adjust (Days (-90.0)) now of
            Just back -> H.liftEff $ EC.setOption (options back (date now) series) ch
            Nothing -> error' "oops, we messed up dates"
        Nothing -> error' "wtf no chart???"
      where
        series = ChartSeries $ makeData <$> group' (extractMonth <$> w)
        makeData :: NonEmpty Array String -> ChartSeriesData
        makeData xs = ChartSeriesData { date: head xs, value: length $ oneOf xs }
        extractMonth :: WatchedData -> String
        extractMonth (WatchedData {created}) =
           unsafePerformEff <<< (prepareDateString <=< JSDate.parse) $ created
        prepareDateString jsdate = do
          year <- toString <$> JSDate.getFullYear jsdate
          month <- show <$> (+) 1 <<< ceil <$> JSDate.getMonth jsdate
          date <- toString <$> JSDate.getDate jsdate
          pure $ intercalate "-" [year, month, date]

    eval :: Query ~> H.ComponentDSL State Query Void (AppEffects eff)
    eval (Init next) = do
      heatmap <- H.getRef (wrap "heatmap")
      case runExcept <<< readHTMLElement <$> heatmap of
        Just (Right el) -> do
          chart <- H.liftEff $ EC.init el
          H.modify _ {chart = Just chart}
        _ -> error' "can't find heatmap element?"
      eval (FetchData next)

    eval (FetchData next) = do
      getResult >>= unV'
        \(Tuple f w) -> do
          H.modify _ {files = f, watched = w}
          updateChart' w
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
        \w -> do
          H.modify _ {watched = w}
          updateChart' w
      pure next

    eval (Filter path next) = do
      case extract path of
        Left e -> error' e *> pure next
        Right s -> eval $ Search s next
      where
        extract (Path s)
          | [_, a] <- split (Pattern "] ") s
          , [b, _] <- split (Pattern " -") a = Right b
          | otherwise = Left "didn't match expected patterns"

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

    eval (ToggleFilterWatched flag next) = do
      H.modify _ {filterWatched = flag}
      pure next

main :: forall e.
  Eff
    ( avar :: AVAR
    , ref :: REF
    , exception :: EXCEPTION
    , dom :: DOM
    , console :: CONSOLE
    , ajax :: AJAX
    , now :: NOW
    , echarts :: ET.ECHARTS
    | e
    )
    Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  log "Running"

options :: Date -> Date -> ChartSeries -> EM.DSL ETP.OptionI
options a b (ChartSeries xs) = do
  E.tooltip do
    E.positionTop
  E.visualMap $ E.continuous do
    E.min 0.0
    E.max 10.0
    E.calculable true
    E.orient ET.Horizontal
    E.leftCenter
    E.topTop
  E.calendar do
    E.calendarSpec do
      E.buildRange do
        E.addDateValue $ a
        E.addDateValue $ b
      E.buildCellSize do
        E.autoValue
        E.autoValue
  E.series do
    E.heatMap do
      E.calendarCoordinateSystem
      E.calendarIndex 0
      E.buildItems
        $ traverse_ E.addItem (pair <$> xs)
  where
    pair (ChartSeriesData {date, value}) = do
      E.buildValues do
        E.addStringValue date
        E.addValue $ toNumber value
