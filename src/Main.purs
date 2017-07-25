module Main where

import Prelude

import Control.IxMonad (ibind, (:*>), (:>>=))
import Control.Monad.Aff (Aff, Canceler, attempt, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Array (filter, sortBy)
import Data.Either (Either(..), either)
import Data.Foreign.Class (class Encode, class Decode, encode)
import Data.Foreign.Generic (decodeJSON)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains, length)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
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
import Node.Buffer (BUFFER, Buffer, create, writeString)
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, defaultSpawnOptions, exec, spawn)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (mkdir, readdir, rename, stat)
import Node.FS.Stats (modifiedTime)
import Node.HTTP (HTTP)
import Node.Path (concat)
import Node.Platform (Platform(..))
import Node.Process (PROCESS, lookupEnv, platform)
import Routes (Route(Route), files, getIcons, open, remove, update, watched)
import SQLite3 (DBConnection, DBEffects, FilePath, newDB, queryDB)
import Types (FileData(..), OpenRequest(..), Path(..), RemoveRequest(..), Success(..))

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

newtype Config = Config
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
  | eff )

ensureDB :: forall eff. FilePath -> Aff (db :: DBEffects | eff) DBConnection
ensureDB path = do
  db <- newDB path
  _ <- queryDB db "CREATE TABLE IF NOT EXISTS watched (path varchar(20) primary key unique, created datetime);" []
  pure db

main :: forall eff.
  Eff (AppEffects (exception :: EXCEPTION | eff))
    (Canceler (AppEffects eff))
main = launchAff $
  (liftEff $ lookupEnv "FILETRACKER_DIR") >>=
  case _ of
    Nothing -> error "we done broke now!!!!"
    Just dir -> do
      db <- ensureDB $ concat [dir, "filetracker"]
      let config = Config {db, dir}
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
      :*> respond' json
    respond' json
      | size <- length json
      , size > 16000 = do
        buffer <- liftEff $ getBuffer size json
        respond buffer
        where
          bind = ibind
    respond' json = respond json
    respondJSON' :: forall req res url
      . IsSymbol url
      => Encode res
      => Route req res url
      -> res
      -> _
    respondJSON' _ = respondJSON <<< unsafeStringify <<< encode
    respondBadRequest e =
      writeStatus statusBadRequest
      :*> headers []
      :*> respond ("bad JSON: " <> show e)
    handleConn conn@{components: Config {dir, db}} = do
      request <- getRequestData
      case Tuple request.method request.url of
        t
          | match t files -> handleFiles files
          | match t watched -> handleWatched watched
          | match t getIcons -> handleGetIcons getIcons
          | match t open -> handleOpen open
          | match t update -> handleUpdate update
          | match t remove -> handleRemove remove
          | otherwise -> fileServer "dist" notFound
        where
          bind = ibind
          match :: forall req res url
            . IsSymbol url
            => Tuple (Either Method CustomMethod) String
            -> Route req res url
            -> Boolean
          match (Tuple m u) (Route {method}) =
            case m of
              Left m' -> m' == method && u == url
              _ -> false
            where
              url = reflectSymbol (SProxy :: SProxy url)
          withBody :: forall req res url
            . Decode req
            => Route req res url
            -> (req -> _)
            -> _
          withBody _ handler = do
            body <- readBody
            either
              respondBadRequest
              handler
              (runExcept $ decodeJSON body)

          handleFiles r = do
            files <- lift' $ readdir' dir
            respondJSON' r files

          handleWatched r = do
            rows <- queryDB' "SELECT path, created FROM watched;" []
            respondJSON $ unsafeStringify rows

          handleOpen r = withBody r \(OpenRequest or) -> do
            _ <- case platform of
              Darwin -> liftEff $ void $ spawn "open" (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
              _ -> liftEff $ exec ("start \"\" \"rust-vlc-finder\" \"" <> concat [dir, unwrap or.path] <>  "\"") defaultExecOptions (const $ pure unit)
            respondJSON' r $ Success {status: "success"}

          handleGetIcons r = withBody r \_ -> do
            _ <- liftEff $ spawn "node" ["get-icons.js"] defaultSpawnOptions
            respondJSON' r $ Success {status: "success"}

          handleUpdate r = withBody r \(FileData ur) -> do
            _ <- if ur.watched
              then queryDB' "INSERT OR REPLACE INTO watched (path, created) VALUES ($1, datetime());" [unwrap ur.path]
              else queryDB' "DELETE FROM watched WHERE path = $1" [unwrap ur.path]
            handleWatched watched

          handleRemove r = withBody r \(RemoveRequest rr) -> do
            let archive = concat [dir, "archive"]
            let name = unwrap rr.path
            let old = concat [dir, name]
            let new = concat [archive, name]
            _ <- lift' <<< attempt $ mkdir archive
            _ <- lift' <<< attempt $ rename old new
            respondJSON' r $ Success {status: "success"}

          queryDB' query params = lift' $ queryDB db query params
