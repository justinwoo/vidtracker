module Main where

import Prelude
import Control.IxMonad (ibind, (:*>), (:>>=))
import Control.Monad.Aff (Aff, Canceler, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Array (filter, sortBy)
import Data.Either (Either(..), either)
import Data.Foreign.Class (class AsForeign, class IsForeign, readJSON, write)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Global.Unsafe (unsafeStringify)
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Request (getRequestData, readBody)
import Hyper.Response (headers, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusNotFound, statusOK)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readdir, stat)
import Node.FS.Stats (modifiedTime)
import Node.HTTP (HTTP)
import Node.Path (concat)
import Node.Platform (Platform(..))
import Node.Process (PROCESS, lookupEnv, platform)
import Routes (Route(Route), files, open, update, watched)
import SQLite3 (DBConnection, DBEffects, FilePath, newDB, queryDB)
import Types (FileData(..), OpenRequest(..), Path(..), Success(..))

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

newtype Config = Config
  { db :: DBConnection
  , dir :: String
  , openExe :: String
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
  | eff )

ensureDB :: forall eff. FilePath -> Aff (db :: DBEffects | eff) DBConnection
ensureDB path = do
  db <- newDB path
  queryDB db "CREATE TABLE IF NOT EXISTS watched (path varchar(20) primary key unique, created datetime);" []
  pure db

main :: forall eff.
  Eff (AppEffects (err :: EXCEPTION | eff))
    (Canceler (AppEffects eff))
main = launchAff $
  (liftEff $ lookupEnv "FILETRACKER_DIR") >>=
  case _ of
    Nothing -> error "we done broke now!!!!"
    Just dir -> do
      db <- ensureDB $ concat [dir, "filetracker"]
      let openExe = case platform of
                      Darwin -> "open"
                      _      -> "explorer"
      let config = Config {db, dir, openExe}
      liftEff $ runServer options config router
  where
    router = getConn :>>= handleConn
    options = defaultOptions { onListening = onListening, onRequestError = onRequestError}
    onListening port = log $ "listening on " <> (show $ unwrap port)
    onRequestError error = log $ "error: " <> show error
    notFound =
      writeStatus statusNotFound
      :*> headers []
      :*> respond (Tuple "<h1>Not Found</h1>" UTF8)
    respondJSON json =
      writeStatus statusOK
      :*> headers [Tuple "Content-Type" "application/json"]
      :*> respond json
    respondJSON' :: forall req res. (AsForeign res) => Route req res -> res -> _
    respondJSON' _ = respondJSON <<< unsafeStringify <<< write
    respondBadRequest e =
      writeStatus statusBadRequest
      :*> headers []
      :*> respond ("bad JSON: " <> show e)
    handleConn conn@{components: Config {dir, db, openExe}} = do
      request <- getRequestData
      case Tuple request.method request.url of
        t
          | match t files -> handleFiles files
          | match t watched -> handleWatched watched
          | match t open -> handleOpen open
          | match t update -> handleUpdate update
          | otherwise -> fileServer "dist" notFound
        where
          bind = ibind
          match :: forall req res. Tuple (Either Method CustomMethod) String -> Route req res -> Boolean
          match (Tuple m u) (Route {method, url}) =
            case m of
              Left m' -> m' == method && u == url
              _ -> false
          withBody :: forall req res. IsForeign req => Route req res -> (req -> _) -> _
          withBody _ handler = do
            body <- readBody
            either
              respondBadRequest
              handler
              (runExcept $ readJSON body)

          handleFiles r = do
            files <- lift' $ readdir' dir
            respondJSON' r files

          handleWatched r = do
            rows <- queryDB' "SELECT path, created FROM watched;" []
            respondJSON $ unsafeStringify rows

          handleOpen r = withBody r \(OpenRequest or) -> do
            _ <- liftEff $ spawn openExe (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
            respondJSON' r $ Success {status: "success"}

          handleUpdate r = withBody r \(FileData ur) -> do
            _ <- if ur.watched
              then queryDB' "INSERT OR REPLACE INTO watched (path, created) VALUES ($1, datetime());" [unwrap ur.path]
              else queryDB' "DELETE FROM watched WHERE path = $1" [unwrap ur.path]
            handleWatched watched

          queryDB' query params = lift' $ queryDB db query params
