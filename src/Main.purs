module Main where

import Prelude

import Config as C
import Control.Monad.Except (ExceptT, except, runExceptT, throwError)
import Data.Array (filter, sortBy)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), contains)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Jajanmen as J
import Makkori as M
import Node.Buffer (Buffer, create, writeString)
import Node.ChildProcess (Exit(..), defaultExecOptions, defaultSpawnOptions, exec, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile, readdir, rename, stat)
import Node.FS.Stats (modifiedTime)
import Node.Path (concat)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Prim.Row as Row
import Record as Record
import Routes (GetRequest, PostRequest, Route, apiRoutes)
import SQLite3 (DBConnection, FilePath, newDB)
import SQLite3 as SQL
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Sunde as Sunde
import Tortellini (parsellIni)
import Type.Prelude (class RowToList, RLProxy(RLProxy))
import Type.Row (Cons, Nil, kind RowList)
import Types (FileData(FileData), GetIconsRequest, OpenRequest(OpenRequest), Path(Path), RemoveRequest(RemoveRequest), Operation(Operation), WatchedData(..))

data Error
  = ServerError String
  | UserError String

prepareResult :: forall a. WriteForeign a => Either Error a -> {status :: Int, response :: String}
prepareResult (Left (UserError error)) = {status: 400, response: writeJSON {error}}
prepareResult (Left (ServerError error)) = {status: 500, response: writeJSON {error}}
prepareResult (Right result) = {status: 200, response: writeJSON result}

readdir' :: String -> Aff (Array Path)
readdir' path = do
  withStats <- traverse pairWithStat =<< filter (contains (Pattern "mkv"))
                                     <$> readdir path
  pure $ Path <<< fst <$> sortByDate withStats
  where
    pairWithStat file = do
      s <- stat $ concat [path, file]
      pure (Tuple file s)
    sortByDate = sortBy <<< flip $ comparing (modifiedTime <<< snd)

getBuffer :: Int -> String -> Effect Buffer
getBuffer size json = do
  buffer <- create size
  _ <- writeString UTF8 0 size json buffer
  pure buffer

type Config =
  { db :: DBConnection
  , dir :: String
  }

class GetFiles m where
  getFiles :: Config -> m (Array Path)

instance gfAF :: GetFiles (ExceptT Error Aff) where
  getFiles {dir} = liftAff $ readdir' dir

class GetWatched m where
  getWatchedData :: Config -> m (Array WatchedData)

instance gwA :: GetWatched (ExceptT Error Aff) where
  getWatchedData {db} = do
    results <- liftAff $ SQL.queryDB db "select * from watched" []
    case read results of
      Right xs -> do
        pure $ WatchedData <$> xs
      Left e -> do
        throwError <<< ServerError $ "getWatchedData:" <> show e

class GetIcons m where
  getIconsData :: Config -> GetIconsRequest -> m Operation

instance giA :: GetIcons (ExceptT Error Aff) where
  getIconsData {db} _ = do
    result <- liftAff $ Sunde.spawn "node" ["get-icons.js"] defaultSpawnOptions
    pure $ case result.exit of
      Normally 0 -> Operation {success: true}
      _ -> Operation {success: false}

class UpdateWatched m where
  updateWatched :: Config -> FileData -> m (Array WatchedData)

instance uwA :: UpdateWatched (ExceptT Error Aff) where
  updateWatched config@{db} (FileData ur) = do
    let params = {"$path": unwrap ur.path}
    _ <- liftAff $ if ur.watched
      then do
        let queryString = SProxy :: SProxy "insert or replace into watched (path, created) values ($path, datetime())"
        void $ J.queryDB db queryString params
      else do
        let queryString = SProxy :: SProxy "delete from watched where path = $path"
        void $ J.queryDB db queryString params
    getWatchedData config

class OpenFile m where
  openFile :: Config -> OpenRequest -> m (Operation)

instance ofA :: OpenFile (ExceptT Error Aff) where
  openFile {dir} (OpenRequest or) = do
    let
      simpleOpen = case platform of
        Just Linux -> Just "xdg-open"
        Just Darwin -> Just "open"
        _ -> Nothing
    _ <- liftAff $ case simpleOpen of
          Just command -> liftEffect $ void $ spawn command (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
          _ -> liftEffect $ void $ exec ("start \"\" \"rust-vlc-finder\" \"" <> concat [dir, unwrap or.path] <>  "\"") defaultExecOptions (const $ pure unit)
    pure $ Operation {success: true}

class RemoveFile m where
  removeFile :: Config -> RemoveRequest -> m (Operation)

instance rfA :: RemoveFile (ExceptT Error Aff) where
  removeFile {dir} (RemoveRequest rr) = do
    let archive = concat [dir, "archive"]
    let name = unwrap rr.path
    let old = concat [dir, name]
    let new = concat [archive, name]
    _ <- liftAff $ do
      void $ attempt $ mkdir archive
      void $ attempt $ rename old new
    pure $ Operation {success: true}

ensureDB :: FilePath -> Aff DBConnection
ensureDB path = do
  db <- newDB path
  _ <- SQL.queryDB db "create table if not exists watched (path text primary key unique, created datetime)" []
  pure db

registerRoutes :: forall routes handlers routesL handlersL app m
   . RowToList routes routesL
  => RowToList handlers handlersL
  => Monad m
  => RoutesHandlers routesL handlersL routes handlers app m
  => Record routes
  -> Record handlers
  -> app
  -> m Unit
registerRoutes routes handlers app =
  registerRoutesImpl
    (RLProxy :: RLProxy routesL)
    (RLProxy :: RLProxy handlersL)
    routes
    handlers
    app

class RoutesHandlers
  (routesL :: RowList)
  (handlersL :: RowList)
  (routes :: # Type)
  (handlers :: # Type)
  app
  m
  where
    registerRoutesImpl :: forall proxy
       . Monad m
      => proxy routesL
      -> proxy handlersL
      -> Record routes
      -> Record handlers
      -> app
      -> m Unit

instance routesHandlersNil :: RoutesHandlers Nil Nil trash1 trash2 app m where
  registerRoutesImpl _ _ _ _ _ = pure unit

instance routesHandlersCons ::
  ( RoutesHandlers rTail hTail routes handlers app m
  , IsSymbol name
  , Row.Cons name handler trash1 handlers
  , Row.Cons name route trash2 routes
  , RegisterHandler route handler app m
  ) => RoutesHandlers (Cons name route rTail) (Cons name handler hTail) routes handlers app m where
  registerRoutesImpl _ _ routes handlers app = do
    registerHandlerImpl (Record.get nameP routes) (Record.get nameP handlers) app
    registerRoutesImpl (RLProxy :: RLProxy rTail) (RLProxy :: RLProxy hTail) routes handlers app
    where
      nameP = SProxy :: SProxy name

class RegisterHandler route handler app m
  | route -> handler app m
  where
    registerHandlerImpl :: route -> handler -> app -> m Unit

instance registerHandlerPost ::
  ( IsSymbol url
  , ReadForeign req
  , WriteForeign res
  ) => RegisterHandler
         (Route PostRequest req res url)
         (req -> ExceptT Error Aff res)
         M.App
         Aff where
  registerHandlerImpl route handler app =
    liftEffect $ M.post (M.Path route') (M.makeHandler handler') app
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' req res = do
        body <- M.getBody req
        launchAff_ do
          {status, response} <- prepareResult <$> runExceptT do
            r :: req <- except <<< lmap (UserError <<< show) $ read body
            handler r
          liftEffect do
            M.setStatus status res
            M.sendResponse response res

instance registerHandlerGet ::
  ( IsSymbol url
  , WriteForeign res
  ) => RegisterHandler
         (Route GetRequest Void res url)
         (ExceptT Error Aff res)
         M.App
         Aff where
  registerHandlerImpl route handler app =
    liftEffect $ M.get (M.Path route') (M.makeHandler handler') app
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' _ res = launchAff_ do
        {status, response} <- prepareResult <$> runExceptT handler
        liftEffect do
          M.setStatus status res
          M.sendResponse response res

main :: Effect Unit
main = launchAff_ do
  dir' <- parsellIni <$> readTextFile UTF8 "./config.ini"
  case dir' of
    Left e -> log $ "We broke: " <> show e
    Right ({vidtracker: {dir}} :: C.Config) -> do
      db <- ensureDB $ concat [dir, "filetracker"]
      let config = {db, dir}
      app <- liftEffect M.makeApp

      middlewares <- liftEffect $ sequence
        [ M.makeJSONMiddleware {}
        , M.makeStaticMiddleware (M.Path "dist") {}
        ]
      liftEffect $ traverse_ (flip (M.use (M.Path "/")) app) middlewares

      registerRoutes
        apiRoutes
        { files: getFiles config
        , watched: getWatchedData config
        , getIcons: getIconsData config
        , update: updateWatched config
        , open: openFile config
        , remove: removeFile config
        }
        app

      _ <- liftEffect $ M.listen (M.Port 3000) (log "Started server") app
      pure unit
