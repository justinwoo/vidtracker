module FrontEnd.Chart where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML.Types (readHTMLElement)
import Data.Array (group', intercalate, length)
import Data.Date (Date)
import Data.DateTime (adjust, date)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), either)
import Data.Int (ceil, toNumber)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, head, oneOf)
import Data.Number.Format (toString)
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse_)
import ECharts.Chart as EC
import ECharts.Commands as E
import ECharts.Monad (interpret)
import ECharts.Monad as EM
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (WatchedData(WatchedData))

type Input = Array WatchedData

newtype ChartSeries = ChartSeries (Array ChartSeriesData)
newtype ChartSeriesData = ChartSeriesData
  { date :: String
  , value :: Int
  }

type State =
  { chart :: Maybe ET.Chart
  , watched :: Array WatchedData
  }

data Query a
  = Init a
  | UpdateChart (Array WatchedData) a

type Effects eff =
  Aff
  ( console :: CONSOLE
  , dom :: DOM
  , echarts :: ET.ECHARTS
  , exception :: EXCEPTION
  , now :: NOW
  | eff )

component :: forall eff. H.Component HH.HTML Query Input Void (Effects eff)
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input UpdateChart
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState =
      { chart: Nothing
      , watched: _
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.ref $ wrap "heatmap"
        , HP.class_ $ wrap "heatmap"]
        []

    error' = H.liftEff <<< error

    updateChart' w = do 
      result <- runExceptT do
        chart <- note "couldn't find existing chart instance" =<< lift (H.gets _.chart)
        now <- lift $ toDateTime <$> now'
        back <- note "somehow calculating time is too hard" $ date <$> adjust (Days (-120.0)) now
        H.liftEff $ EC.setOption (interpret $ options back (date now) series) chart
      either error' pure result
      where
        note :: forall m e. MonadThrow e m => e -> Maybe ~> m
        note s = maybe (throwError s) pure
        now' = H.liftEff now
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

    eval :: Query ~> H.ComponentDSL State Query Void (Effects eff)
    eval (Init next) = do
      heatmap <- H.getRef (wrap "heatmap")
      case runExcept <<< readHTMLElement <$> heatmap of
        Just (Right el) -> do
          chart <- H.liftEff $ EC.init el
          H.modify _ {chart = Just chart}
        _ -> error' "can't find heatmap element?"
      eval (UpdateChart [] next)

    eval (UpdateChart w next) = do
      updateChart' w
      pure next

options :: Date -> Date -> ChartSeries -> EM.DSL' ETP.OptionI
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
