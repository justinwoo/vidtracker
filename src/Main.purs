module Main where

import Prelude

import Chanpon (Table(..), createTableIfNotExists, deleteFrom, insertOrReplaceInto, selectAll)
import Config as C
import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, except, runExceptT, throwError)
import Data.Array (filter, sortBy)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.JSDate (now, toISOString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Record (get)
import Data.String (Pattern(Pattern), contains)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Makkori as M
import Node.Buffer (BUFFER, Buffer, create, writeString)
import Node.ChildProcess (CHILD_PROCESS, Exit(..), defaultExecOptions, defaultSpawnOptions, exec, spawn)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (mkdir, readTextFile, readdir, rename, stat)
import Node.FS.Stats (modifiedTime)
import Node.HTTP (HTTP)
import Node.Path (concat)
import Node.Platform (Platform(..))
import Node.Process (PROCESS, platform)
import Routes (GetRequest, PostRequest, Route, apiRoutes)
import SQLite3 (DBConnection, DBEffects, FilePath, newDB)
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Sunde as Sunde
import Tortellini (parseIni)
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

watchedTable :: Table
watchedTable = Table "watched"

readdir' :: forall eff.
  String
  -> Aff
       ( fs :: FS
       | eff
       )
       (Array Path)
readdir' path = do
  withStats <- traverse pairWithStat =<< filter (contains (Pattern "mkv"))
                                     <$> readdir path
  pure $ Path <<< fst <$> sortByDate withStats
  where
    pairWithStat file = do
      s <- stat $ concat [path, file]
      pure (Tuple file s)
    sortByDate = sortBy <<< flip $ comparing (modifiedTime <<< snd)

getBuffer :: forall e.
  Int
  -> String
  -> Eff
    ( buffer :: BUFFER
    | e
    )
    Buffer
getBuffer size json = do
  buffer <- create size
  _ <- writeString UTF8 0 size json buffer
  pure buffer

type Config =
  { db :: DBConnection
  , dir :: String
  }

type AppEffects eff =
  ( avar :: AVAR
  , console :: CONSOLE
  , db :: DBEffects
  , process :: PROCESS
  , cp :: CHILD_PROCESS
  , http :: HTTP
  , fs :: FS
  , buffer :: BUFFER
  | eff
  )

class GetFiles m where
  getFiles :: Config -> m (Array Path)

instance gfAF ::
  ( MonadAff (fs :: FS | trash) (Aff e)
  ) => GetFiles (ExceptT Error (Aff e)) where
  getFiles {dir} = liftAff $ readdir' dir

class GetWatched m where
  getWatchedData :: Config -> m (Array WatchedData)

instance gwA ::
  ( MonadAff (db :: DBEffects | trash) (Aff e)
  ) => GetWatched (ExceptT Error (Aff e)) where
  getWatchedData {db} = do
    results <- liftAff $ selectAll watchedTable db
    case runExcept $ sequence results of
      Right xs -> do
        pure $ WatchedData <$> xs
      Left e -> do
        throwError <<< ServerError $ "getWatchedData:" <> show e

class GetIcons m where
  getIconsData :: Config -> GetIconsRequest -> m Operation

instance giA ::
  ( MonadAff (cp :: CHILD_PROCESS, ref :: REF, exception :: EXCEPTION | trash) (Aff e)
  ) => GetIcons (ExceptT Error (Aff e)) where
  getIconsData {db} _ = do
    result <- liftAff $ Sunde.spawn "node" ["get-icons.js"] defaultSpawnOptions
    pure $ case result.exit of
      Normally 0 -> Operation {success: true}
      _ -> Operation {success: false}

class UpdateWatched m where
  updateWatched :: Config -> FileData -> m (Array WatchedData)

instance uwA ::
  ( MonadAff (db :: DBEffects | trash) (Aff e)
  ) => UpdateWatched (ExceptT Error (Aff e)) where
  updateWatched config@{db} (FileData ur) = do
    _ <- liftAff $ if ur.watched
      then do
        now' <- liftEff <<< unsafeCoerceEff $ toISOString =<< now
        insertOrReplaceInto watchedTable db {path: ur.path, created: now'}
      else deleteFrom watchedTable db {path: ur.path}
    getWatchedData config

class OpenFile m where
  openFile :: Config -> OpenRequest -> m (Operation)

instance ofA ::
  ( MonadAff (cp :: CHILD_PROCESS | trash) (Aff e)
  ) => OpenFile (ExceptT Error (Aff e)) where
  openFile {dir} (OpenRequest or) = do
    let
      simpleOpen = case platform of
        Just Linux -> Just "xdg-open"
        Just Darwin -> Just "open"
        _ -> Nothing
    _ <- liftAff $ case simpleOpen of
          Just command -> liftEff $ void $ spawn command (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
          _ -> liftEff $ exec ("start \"\" \"rust-vlc-finder\" \"" <> concat [dir, unwrap or.path] <>  "\"") defaultExecOptions (const $ pure unit)
    pure $ Operation {success: true}

class RemoveFile m where
  removeFile :: Config -> RemoveRequest -> m (Operation)

instance rfA ::
  ( MonadAff (fs :: FS | trash) (Aff e)
  ) => RemoveFile (ExceptT Error (Aff e)) where
  removeFile {dir} (RemoveRequest rr) = do
    let archive = concat [dir, "archive"]
    let name = unwrap rr.path
    let old = concat [dir, name]
    let new = concat [archive, name]
    _ <- liftAff $ do
      void $ attempt $ mkdir archive
      void $ attempt $ rename old new
    pure $ Operation {success: true}

ensureDB :: forall eff. FilePath -> Aff (db :: DBEffects | eff) DBConnection
ensureDB path = do
  db <- newDB path
  createTableIfNotExists watchedTable db
    { path: "text primary key unique"
    , created: "datetime"
    }
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
  , RowCons name handler trash1 handlers
  , RowCons name route trash2 routes
  , RegisterHandler route handler app m
  ) => RoutesHandlers (Cons name route rTail) (Cons name handler hTail) routes handlers app m where
  registerRoutesImpl _ _ routes handlers app = do
    registerHandlerImpl (get nameP routes) (get nameP handlers) app
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
         (req -> ExceptT Error (Aff e) res)
         M.App
         (Aff e) where
  registerHandlerImpl route handler app =
    liftEff $ M.post (M.Path route') (M.makeHandler handler') app
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' req res = do
        body <- M.getBody req
        launchAff_ do
          {status, response} <- prepareResult <$> runExceptT do
            r :: req <- except <<< lmap (UserError <<< show) $ read body
            handler r
          liftEff do
            M.setStatus status res
            M.sendResponse response res

instance registerHandlerGet ::
  ( IsSymbol url
  , WriteForeign res
  ) => RegisterHandler
         (Route GetRequest Void res url)
         (ExceptT Error (Aff e) res)
         M.App
         (Aff e) where
  registerHandlerImpl route handler app =
    liftEff $ M.get (M.Path route') (M.makeHandler handler') app
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' _ res = launchAff_ do
        {status, response} <- prepareResult <$> runExceptT handler
        liftEff do
          M.setStatus status res
          M.sendResponse response res

main :: forall e
   . Eff
       ( console :: CONSOLE
       , db :: DBEffects
       , process :: PROCESS
       , cp :: CHILD_PROCESS
       , fs :: FS
       , ref :: REF
       , exception :: EXCEPTION
       | e
       )
       Unit
main = launchAff_ do
  dir' <- parseIni <$> readTextFile UTF8 "./config.ini"
  case dir' of
    Left e -> error $ "We broke: " <> show e
    Right ({vidtracker: {dir}} :: C.Config) -> do
      db <- ensureDB $ concat [dir, "filetracker"]
      let config = {db, dir}
      app <- liftEff M.makeApp

      middlewares <- liftEff $ sequence
        [ M.makeJSONMiddleware {}
        , M.makeStaticMiddleware (M.Path "dist") {}
        ]
      liftEff $ traverse_ (flip (M.use (M.Path "/")) app) middlewares

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

      _ <- liftEff $ M.listen (M.Port 3000) (log "Started server") app
      pure unit
