module FrontEnd where

import Prelude

import CSS (backgroundImage, borderColor, fromHexString, rgb, url)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, errorShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Array (drop, filter, head, reverse, sort, sortWith)
import Data.Either (Either(..), either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Encode, class Decode, encode)
import Data.Foreign.Generic (decodeJSON)
import Data.JSDate as JSDate
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set, insert, member)
import Data.String (Pattern(Pattern), contains, fromCharArray, split, take, toCharArray, toLower)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (find)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import ECharts.Types as ET
import FrontEnd.Chart as Chart
import Global (encodeURIComponent)
import Global.Unsafe (unsafeStringify)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AJ
import Node.Crypto.Hash (Algorithm(..), hex)
import Routes (Route(..), files, getIcons, open, remove, update, watched)
import Types (FileData(..), GetIconsRequest(..), OpenRequest(..), Path(..), RemoveRequest(..), WatchedData(..))

reverse' :: String -> String
reverse' = fromCharArray <<< reverse <<< toCharArray

extractNameKinda :: Path -> Either String String
extractNameKinda (Path s)
  | [_, a] <- split (Pattern "] ") s
  , result <- split (Pattern " - ") (reverse' a)
  , Just b <- head $ drop 1 result = Right (reverse' b)
  | otherwise = Left "didn't match expected patterns"

type VE a = V (NonEmptyList ForeignError) a

request :: forall req res url m eff.
  MonadAff
    ( ajax :: AJAX
    | eff
    )
    m
  => Encode req
  => Decode res
  => IsSymbol url
  => Route req res url -> Maybe req -> m (VE res)
request (Route route) body =
  H.liftAff $ either invalid pure <$> parseResponse <$> action
  where
    action = AJ.affjax $ AJ.defaultRequest
      { method = Left route.method
      , url = reflectSymbol (SProxy :: SProxy url)
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
  , deleteConfirmations :: Set Path
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

type AppEffects eff =
  Aff
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM
  , echarts :: ET.ECHARTS
  , exception :: EXCEPTION
  , now :: NOW
  | eff )

data Slot = ChartSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: forall eff. H.Component HH.HTML Query Unit Void (AppEffects eff)
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
      }

    render :: State -> H.ParentHTML Query Chart.Query Slot (AppEffects eff)
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
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
            [ HP.classes $ wrap <$>
              [ "refresh-files"
              , "pure-button"
              ]
            , HE.onClick <<< HE.input_ $ FetchData
            ]
            [ HH.text "Refresh" ]
        getIconsButton =
          HH.button
            [ HP.classes $ wrap <$>
              [ "get-icons"
              , "pure-button"
              ]
            , HE.onClick <<< HE.input_ $ GetIcons
            ]
            [ HH.text "Run Get Icons" ]
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
              [ HP.class_ $ wrap "dot"
              ] []
            , HH.h3
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
            , HH.h3
              [ HP.class_ $ wrap "delete-link"
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
            [ HH.span
              [ HP.class_ $ wrap "dot"
              , style do
                case extractNameKinda path of
                  Right name ->
                    backgroundImage (url $ "icons/" <> encodeURIComponent name)
                  Left e -> pure mempty
                -- borderColor $ fromMaybe (rgb 255 105 180) dotColor
              ] []
            , HH.a
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
            , HH.button
              [ HP.classes $ wrap <$>
                [ "delete-link"
                , "pure-button"
                , if deleteConfirmation
                    then "delete-confirmation"
                    else ""
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
            getDate (WatchedData {created}) =
              -- parsing date is UTZ dependent (ergo effectful), but in our case, we really don't care
              JSDate.toDateString <<< unsafePerformEff <<< JSDate.parse $ created
            -- -- whatever, we just need the name of the show kind of
            -- hexColor = take 6 $ unsafePerformEff $ hex MD5 (show $ extractNameKinda path)
            -- dotColor = fromHexString $ "#" <> hexColor

    error' = H.liftEff <<< error

    eval :: Query ~> H.ParentDSL State Query Chart.Query Slot Void (AppEffects eff)
    eval (Init next) = do
      eval (FetchData next)

    eval (FetchData next) = do
      getResult >>= unV'
        \(Tuple f w) -> do
          H.modify _ {files = f, watched = w}
      pure next
      where
        getResult = do
          files <- request files Nothing
          watched <- request watched Nothing
          pure $ Tuple <$> files <*> watched

    eval (GetIcons next) = do
      _ <- request getIcons $ Just GetIconsRequest
      pure next

    eval (OpenFile path next) = do
      _ <- request open $ Just (OpenRequest {path})
      pure next

    eval (SetWatched path flag next) = do
      request update (Just $ FileData {path, watched: flag})
        >>= unV' \w -> H.modify _ {watched = w}
      pure next

    eval (Filter path next) = do
      case extractNameKinda path of
        Left e -> error' e *> pure next
        Right s -> eval $ Search s next

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

    eval (ConfirmDeletion path next) = do
      H.modify \s -> s {deleteConfirmations = insert path s.deleteConfirmations}
      pure next

    eval (Delete path next) = do
      _ <- request remove $  Just (RemoveRequest {path})
      eval (FetchData next)

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
