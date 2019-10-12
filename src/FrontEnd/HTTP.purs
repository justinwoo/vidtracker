module FrontEnd.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import FrontEnd.Types (Request(..), Response)
import FrontEnd.Window (refreshPage)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Routes (GetRoute, PostRoute, apiRoutes)
import Simple.JSON as JSON
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Types (Path(..))

prefixUrl :: String -> M.URL
prefixUrl url = M.URL $ "http://localhost:4567" <> url

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

http :: Event Request -> Effect (Event Response)
http actions = do
  {event, push} <- Event.create
  let push' = liftEffect <<< push
  _ <- Event.subscribe actions (launchAff_ <<< handleRequest push')
  pure event

handleRequest :: (Response -> Aff Unit) -> Request -> Aff Unit
handleRequest push FetchFilesRequest = do
  attempt <- getData
  case attempt of
    Right r -> push r
    Left e -> Console.error $ "Failed to fetch data: " <> show e

  where
    getData = do
      filesData <- get apiRoutes.files
      watchedData <- get apiRoutes.watched
      pure $ { filesData: _, watchedData: _ }
        <$> filesData
        <*> watchedData

handleRequest push FetchIconsRequest = do
  Console.log "Fetching icons..."
  void $ post apiRoutes.getIcons {}
  liftEffect refreshPage

handleRequest push (OpenFileRequest file) = do
  Console.log $ "Opening file: " <> un Path file.name
  void $ post apiRoutes.open { path: file.name }

handleRequest push (MarkFileRequest file) = do
  Console.log $ "Updating " <> un Path file.name
  void $ post apiRoutes.update
    { path: file.name
    , watched: maybe true (const false) file.watched
    }
  handleRequest push FetchFilesRequest
