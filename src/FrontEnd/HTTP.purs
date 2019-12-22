module FrontEnd.HTTP where

import Prelude

import Bonjiri as B
import Calpis as C
import Calpis.Impl.Window (windowFetch)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import FrontEnd.Types (Request(..), Response)
import FrontEnd.Window (refreshPage)
import Routes (GetRoute, PostRoute, apiRoutes)
import Simple.JSON as JSON
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Types (Path(..))

prefixUrl :: String -> C.URL
prefixUrl url = C.URL $ "http://localhost:4567" <> url

get :: forall res url. JSON.ReadForeign res => IsSymbol url => GetRoute res url -> B.PromiseSpec (JSON.E res)
get _ = JSON.read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = C.fetch windowFetch
    action = B.chain C.json (fetch (prefixUrl url) C.defaultFetchOptions)

post :: forall req res url. JSON.WriteForeign req => JSON.ReadForeign res => IsSymbol url =>
  PostRoute req res url -> req -> B.PromiseSpec (JSON.E res)
post _ body = JSON.read <$> action
  where
    url = reflectSymbol (SProxy :: SProxy url)
    fetch = C.fetch windowFetch
    options =
      { method: C.postMethod
      , headers: C.makeHeaders { "Content-Type": "application/json" }
      , body: JSON.writeJSON body
      }
    action = B.chain C.json (fetch (prefixUrl url) options)

http :: Event Request -> Effect (Event Response)
http actions = do
  {event, push} <- Event.create
  let push' = liftEffect <<< push
  _ <- Event.subscribe actions (handleRequest push')
  pure event

handleRequest :: (Response -> Effect Unit) -> Request -> Effect Unit
handleRequest push FetchFilesRequest = do
  B.run
    do \e -> Console.error $ "Failed to fetch data"
    do
      \result -> case result of
        Right r -> push r
        Left e -> Console.error $ "Failed to parse data: " <> show e
    getData

  where
    getData =
      let
        getFilesData = get apiRoutes.files
        getWatchedData = get apiRoutes.watched
      in
        (lift2 { filesData: _, watchedData: _ })
          <$> getFilesData
          <*> getWatchedData

handleRequest push FetchIconsRequest = do
  Console.log "Fetching icons..."
  B.run
    do \e -> Console.error "Error fetching icons"
    do \_ -> refreshPage
    (post apiRoutes.getIcons {})

handleRequest push (OpenFileRequest file) = do
  Console.log $ "Opening file: " <> un Path file.name
  B.run
    do \e -> Console.error "Error opening file"
    mempty
    (post apiRoutes.open { path: file.name })

handleRequest push (MarkFileRequest file) = do
  Console.log $ "Updating " <> un Path file.name
  let
    update =
      post apiRoutes.update
        { path: file.name
        , watched: maybe true (const false) file.watched
        }
  B.run
    do \_ -> Console.error "Error fetching files"
    do \_ -> handleRequest push FetchFilesRequest
    update
