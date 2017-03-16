module Main where

import Prelude
import Data.Foreign.Generic as DFG
import Control.IxMonad (ibind, (:*>), (:>>=))
import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Array (sortBy)
import Data.Either (Either(..))
import Data.Foreign.Class (class IsForeign, readJSON, write)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), fst)
import Global.Unsafe (unsafeStringify)
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptions, runServer)
import Hyper.Request (readBody)
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
import Node.Process (PROCESS, lookupEnv)
import SQLite3 (DBEffects, newDB, queryDB)

newtype OpenRequest = OpenRequest
  { path :: String
  }
derive instance grOR :: Generic OpenRequest _
instance ifOR :: IsForeign OpenRequest where
  read = DFG.readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

newtype UpdateRequest = UpdateRequest
  { path :: String
  , watched :: Boolean
  }
derive instance grUR :: Generic UpdateRequest _
instance ifUR :: IsForeign UpdateRequest where
  read = DFG.readGeneric $ DFG.defaultOptions {unwrapSingleConstructors = true}

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

main :: forall eff.
  Eff (AppEffects (err :: EXCEPTION | eff))
    (Canceler (AppEffects eff))
main = launchAff $
  (liftEff $ lookupEnv "FILETRACKER_DIR") >>=
  case _ of
    Nothing -> error "we done broke now!!!!"
    Just dir -> do
      db <- newDB $ concat [dir, "filetracker"]
      liftEff $ runServer options {} (router {dir, db})
  where
    router ctx = getConn :>>= handleConn ctx
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
    readFiles path = lift' $ unsafeStringify <<< write <$> readdir'
      where
        readdir' = do
          withStats <- traverse pairWithStat =<< readdir path
          pure $ fst <$> sortByDate withStats
        pairWithStat file = do
          s <- stat $ concat [path, file]
          pure (Tuple file s)
        sortByDate = sortBy compareDates
        compareDates (Tuple _ a) (Tuple _ b) =
          compare EQ $ compare (modifiedTime a) (modifiedTime b)
    handleConn {dir, db} conn =
      case Tuple conn.request.method conn.request.url of
        Tuple (Left GET) "/api/files" -> files
        Tuple (Left GET) "/api/watched" -> watched
        Tuple (Left POST) "/api/update" -> update
        Tuple (Left POST) "/api/open" -> open
        _ -> fileServer "dist" notFound
        where
          bind = ibind
          files = readFiles dir :>>= respondJSON
          queryDB' q p = lift' $ queryDB db q p
          open = do
            body <- readBody
            case runExcept $ readJSON body of
              Right (OpenRequest or) -> do
                _ <- liftEff $ spawn "explorer" (pure $ concat [dir, or.path]) defaultSpawnOptions
                respondJSON "{}"
              Left e -> do
                writeStatus statusBadRequest
                headers []
                respond $ "you gave me bad JSON!!!\n" <> show e <> "\nin\n" <> body
          update = do
            body <- readBody
            case runExcept $ readJSON body of
              Right (UpdateRequest ur) -> do
                _ <- if ur.watched
                  then queryDB' "INSERT OR REPLACE INTO watched (path, created) VALUES ($1, datetime());" [ur.path]
                  else queryDB' "DELETE FROM watched WHERE path = $1" [ur.path]
                watched
              Left e -> do
                writeStatus statusBadRequest
                headers []
                respond $ "you gave me bad JSON!!!\n" <> show e <> "\nin\n" <> body
          watched = do
            -- should come back as [{path :: String, created :: String}]
            -- demand refund if not
            a <- queryDB' "SELECT path, created FROM watched;" []
            respondJSON $ unsafeStringify a
