module FrontEnd.Chart where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (group', intercalate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (adjust)
import Data.DateTime as DT
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Enum (fromEnum)
import Data.Int (ceil)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (wrap)
import Data.Number.Format (toString)
import Data.Time.Duration (Days(..))
import Effect.Aff (Aff)
import Effect.Console (error)
import Effect.Now (now)
import Effect.Unsafe (unsafePerformEffect)
import Gomtang.Basic as Gom
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record.Format as RF
import Type.Prelude (SProxy(..))
import Types (WatchedData)

type Input = Array WatchedData

newtype ChartSeries = ChartSeries (Array ChartSeriesData)
newtype ChartSeriesData = ChartSeriesData
  { date :: String
  , value :: Int
  }

type State =
  { chart :: Maybe Gom.Instance
  , watched :: Array WatchedData
  }

data Query a
  = Init a
  | UpdateChart (Array WatchedData) a

component :: H.Component HH.HTML Query Input Void Aff
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

    error' = H.liftEffect <<< error

    updateChart' w = do
      result <- runExceptT do
        chart <- note "couldn't find existing chart instance" =<< lift (H.gets _.chart)
        now <- lift $ toDateTime <$> now'
        back <- note "somehow calculating time is too hard" $ adjust (Days (-120.0)) now
        H.liftEffect $ Gom.setOption (options back now series) chart
      either error' pure result
      where
        note :: forall m e. MonadThrow e m => e -> Maybe ~> m
        note s = maybe (throwError s) pure
        now' = H.liftEffect now
        series = ChartSeries $ makeData <$> group' (extractMonth <$> w)
        makeData :: NonEmptyArray String -> ChartSeriesData
        makeData xs = ChartSeriesData { date: NEA.head xs, value: NEA.length xs }
        extractMonth :: WatchedData -> String
        extractMonth {created} =
           unsafePerformEffect <<< (prepareDateString <=< JSDate.parse) $ created
        prepareDateString jsdate = do
          year <- toString <$> JSDate.getFullYear jsdate
          month <- show <$> (+) 1 <<< ceil <$> JSDate.getMonth jsdate
          date <- toString <$> JSDate.getDate jsdate
          pure $ intercalate "-" [year, month, date]

    eval :: Query ~> H.ComponentDSL State Query Void Aff
    eval (Init next) = do
      heatmap <- H.getRef (wrap "heatmap")
      case heatmap of
        Just el -> do
          chart <- H.liftEffect $ Gom.makeChart el
          H.modify_ _ {chart = Just chart}
        _ -> error' "can't find heatmap element?"
      eval (UpdateChart [] next)

    eval (UpdateChart w next) = do
      updateChart' w
      pure next

    options a b (ChartSeries xs) =
      { tooltip: Gom.makeTooltip { position: "top" }
      , visualMap: Gom.makeVisualMap
          { min: 0.0
          , max: 10.0
          , calculable: true
          }
      , calendar: Gom.makeCalendar
          { range: formatDate <$> [a, b]
          , cellSize: ["auto", "auto"]
          }
      , series: pure $ Gom.makeHeatMapSeries $
          { coordinateSystem: "calendar"
          , calendarIndex: 0
          , data: prepareSeriesData <$> xs
        }
      }
      where
        prepareSeriesData (ChartSeriesData {date, value}) =
          [date, show value]
        formatDate datetime
          | d <- DT.date datetime
          = RF.format (SProxy :: SProxy "{year}-{month}-{day}")
              { year: show $ fromEnum (DT.year d)
              , month: show $ fromEnum (DT.month d)
              , day: show $ fromEnum (DT.day d)
              }
