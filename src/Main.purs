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
import Shoronpo (formatSymbol, intercalateRecordLabels, intercalateRowLabels, intercalateRowValues)
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Sunde as Sunde
import Tortellini (parsellIni)
import Type.Prelude (class RowToList, Proxy(..), RLProxy(RLProxy))
import Type.Row (Cons, Nil, kind RowList)
import Types (FileData, GetIconsRequest, OpenRequest, Operation, Path(Path), RemoveRequest, WatchedData)

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
  , exe :: String
  }

class GetFiles m where
  getFiles :: Config -> m (Array Path)

instance gfAF :: GetFiles (ExceptT Error Aff) where
  getFiles {dir} = liftAff $ readdir' dir

class GetWatched m where
  getWatchedData :: Config -> m (Array WatchedData)

instance gwA :: GetWatched (ExceptT Error Aff) where
  getWatchedData {db} = do
    let
      template = SProxy :: SProxy "select {columns} from watched order by created desc"
      query = formatSymbol template
        { columns: intercalateRecordLabels (Proxy :: Proxy WatchedData) (SProxy :: SProxy ", ") }
    results <- liftAff $ J.queryDB db query {}
    case read results of
      Right xs -> do
        pure xs
      Left e -> do
        throwError <<< ServerError $ "getWatchedData:" <> show e

class GetIcons m where
  getIconsData :: Config -> GetIconsRequest -> m Operation

instance giA :: GetIcons (ExceptT Error Aff) where
  getIconsData {db} _ = do
    result <- liftAff $ Sunde.spawn "node" ["get-icons.js"] defaultSpawnOptions
    pure $ case result.exit of
      Normally 0 -> {success: true}
      _ -> {success: false}

class UpdateWatched m where
  updateWatched :: Config -> FileData -> m (Array WatchedData)

instance uwA :: UpdateWatched (ExceptT Error Aff) where
  updateWatched config@{db} ur = do
    let params = {"$path": unwrap ur.path}
    _ <- liftAff $ if ur.watched
      then do
        let
          fields =
            { path: SProxy :: SProxy "$path"
            , created: SProxy :: SProxy "datetime()"
            }
          template = SProxy :: SProxy "insert or replace into watched ({fields}) values ({values})"
          query = formatSymbol template
            { fields: intercalateRowLabels fields (SProxy :: SProxy ", ")
            , values: intercalateRowValues fields (SProxy :: SProxy ", ")
            }
        void $ J.queryDB db query params
      else do
        let queryString = SProxy :: SProxy "delete from watched where path = $path"
        void $ J.queryDB db queryString params
    getWatchedData config

class OpenFile m where
  openFile :: Config -> OpenRequest -> m (Operation)

instance ofA :: OpenFile (ExceptT Error Aff) where
  openFile {dir, exe} or = do
    let
      simpleOpen = case exe, platform of
        exe', _ | exe' /= "" -> Just exe'
        _, Just Linux -> Just "xdg-open"
        _, Just Darwin -> Just "open"
        _, _ -> Nothing
    _ <- liftAff $ case simpleOpen of
          Just command -> liftEffect $ void $ spawn command (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
          _ -> liftEffect $ void $ exec ("start \"\" \"rust-vlc-finder\" \"" <> concat [dir, unwrap or.path] <>  "\"") defaultExecOptions (const $ pure unit)
    pure $ {success: true}

class RemoveFile m where
  removeFile :: Config -> RemoveRequest -> m (Operation)

instance rfA :: RemoveFile (ExceptT Error Aff) where
  removeFile {dir} rr = do
    let archive = concat [dir, "archive"]
    let name = unwrap rr.path
    let old = concat [dir, name]
    let new = concat [archive, name]
    _ <- liftAff $ do
      void $ attempt $ mkdir archive
      void $ attempt $ rename old new
    pure $ {success: true}

ensureDB :: FilePath -> Aff DBConnection
ensureDB path = do
  db <- newDB path
  _ <- SQL.queryDB db "create table if not exists watched (path text primary key unique, created datetime)" []
  pure db

registerRoutes :: forall routes handlers routesL
   . RowToList routes routesL
  => RoutesHandlers routesL routes handlers
  => Record routes
  -> Record handlers
  -> M.App
  -> Effect Unit
registerRoutes = registerRoutesImpl (RLProxy :: RLProxy routesL)

class RoutesHandlers
  (routesL :: RowList)
  (routes :: # Type)
  (handlers :: # Type)
  | routesL -> routes handlers
  where
    registerRoutesImpl :: forall proxy
       . proxy routesL
      -> Record routes
      -> Record handlers
      -> M.App
      -> Effect Unit

instance routesHandlersNil :: RoutesHandlers Nil routes handlers where
  registerRoutesImpl _ _ _ _ = pure unit

instance routesHandlersCons ::
  ( RoutesHandlers tail routes handlers
  , IsSymbol name
  , Row.Cons name handler handlers' handlers
  , Row.Cons name route routes' routes
  , RegisterHandler route handler
  ) => RoutesHandlers (Cons name route tail) routes handlers where
  registerRoutesImpl _ routes handlers app = do
    registerHandlerImpl (Record.get nameP routes) (Record.get nameP handlers) app
    registerRoutesImpl (RLProxy :: RLProxy tail) routes handlers app
    where
      nameP = SProxy :: SProxy name

class RegisterHandler route handler
  | route -> handler
  where
    registerHandlerImpl :: route -> handler -> M.App -> Effect Unit

instance registerHandlerPost ::
  ( IsSymbol url
  , ReadForeign req
  , WriteForeign res
  ) => RegisterHandler
         (Route PostRequest req res url)
         (req -> ExceptT Error Aff res)
         where
  registerHandlerImpl route handler app =
    M.post (M.Path route') (M.makeHandler handler') app
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
         where
  registerHandlerImpl route handler app =
    M.get (M.Path route') (M.makeHandler handler') app
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
    Right ({vidtracker} :: C.Config) -> do
      db <- ensureDB $ concat [vidtracker.dir, "filetracker"]
      let config = Record.merge vidtracker {db}
      _ <- liftEffect do
        app <- M.makeApp

        middlewares <- sequence
          [ M.makeJSONMiddleware {}
          , M.makeStaticMiddleware (M.Path "dist") {}
          ]
        traverse_ (flip (M.use (M.Path "/")) app) middlewares
        M.use (M.Path "*") accessControlMiddleware app

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

        _ <- M.listen (M.Port 3000) (log "Started server") app
        pure unit
      pure unit

foreign import accessControlMiddleware :: M.Middleware
