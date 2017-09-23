module Main where

import Prelude

import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, sortBy)
import Data.Either (Either(Left, Right))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), contains)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Node.Buffer (BUFFER, Buffer, create, writeString)
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, defaultSpawnOptions, exec, spawn)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, AppM, listenHttp, use, useExternal)
import Node.Express.App as E
import Node.Express.Handler (HandlerM(HandlerM))
import Node.Express.Middleware.Static (static)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (ExpressM, Request, Response)
import Node.FS (FS)
import Node.FS.Aff (mkdir, readdir, rename, stat)
import Node.FS.Stats (modifiedTime)
import Node.HTTP (HTTP)
import Node.Path (concat)
import Node.Platform (Platform(..))
import Node.Process (PROCESS, lookupEnv, platform)
import Routes (GetRoute, PostRoute, files, getIcons, open, remove, update, watched)
import SQLite3 (DBConnection, DBEffects, FilePath, newDB, queryDB)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Types (FileData(FileData), GetIconsRequest, OpenRequest(OpenRequest), Path(Path), RemoveRequest(RemoveRequest), Success(Success), WatchedData)
import Unsafe.Coerce (unsafeCoerce)

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

data CommandsF next
  = GetFiles (Array Path -> next)
  | GetWatched (Array WatchedData -> next)
  | GetIcons GetIconsRequest (Success -> next)
  | UpdateWatched FileData (Array WatchedData -> next)
  | OpenFile OpenRequest (Success -> next)
  | RemoveFile RemoveRequest (Success -> next)
  | ReturnSuccess (Success -> next)
derive instance fCF :: Functor CommandsF

interpret :: Config -> Free CommandsF ~> HandlerM _
interpret config free = do
  liftAff $ runReaderT (foldFree evalCommand free) config

evalCommand :: CommandsF ~> ReaderT Config (Aff _)
evalCommand (GetFiles f) = do
  {dir} <- ask
  files <- lift (readdir' dir)
  pure $ f files
evalCommand (GetWatched f) = do
  {db} <- ask
  watchedData :: Array WatchedData <- lift $ unsafeCoerce <$>
    queryDB db "SELECT path, created FROM watched;" []
  pure $ f watchedData
evalCommand (GetIcons _ f) = do
  _ <- liftEff $ spawn "node" ["get-icons.js"] defaultSpawnOptions
  evalCommand (ReturnSuccess f)
evalCommand (ReturnSuccess f) = do
  pure <<< f $ Success {status: "success"}
evalCommand (UpdateWatched (FileData ur) f) = do
  {db} <- ask
  _ <- lift $ if ur.watched
    then queryDB db "INSERT OR REPLACE INTO watched (path, created) VALUES ($1, datetime());" [unwrap ur.path]
    else queryDB db "DELETE FROM watched WHERE path = $1" [unwrap ur.path]
  evalCommand (GetWatched f)
evalCommand (OpenFile (OpenRequest or) f) = do
  {dir} <- ask
  _ <- lift $ case platform of
        Just Darwin -> liftEff $ void $ spawn "open" (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
        _ -> liftEff $ exec ("start \"\" \"rust-vlc-finder\" \"" <> concat [dir, unwrap or.path] <>  "\"") defaultExecOptions (const $ pure unit)
  evalCommand (ReturnSuccess f)
evalCommand (RemoveFile (RemoveRequest rr) f) = do
  {dir} <- ask
  let archive = concat [dir, "archive"]
  let name = unwrap rr.path
  let old = concat [dir, name]
  let new = concat [archive, name]
  _ <- liftAff do
    void $ attempt $ mkdir archive
    void $ attempt $ rename old new
  evalCommand (ReturnSuccess f)

ensureDB :: forall eff. FilePath -> Aff (db :: DBEffects | eff) DBConnection
ensureDB path = do
  db <- newDB path
  _ <- queryDB db "CREATE TABLE IF NOT EXISTS watched (path varchar(20) primary key unique, created datetime);" []
  pure db

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

foreign import _getBody :: Request -> Foreign

getBody :: forall e. HandlerM e Foreign
getBody = HandlerM \req _ _ -> pure $ _getBody req

handleGet :: forall f next res url
   . Category f
  => IsSymbol url
  => WriteForeign res
  => Config
  -> GetRoute res url
  -> (f next next -> CommandsF res)
  -> AppM _ Unit
handleGet config route command =
  E.get route handler'
  where
    route = reflectSymbol (SProxy :: SProxy url)
    handler' = do
      response <- interpret config $ liftF (command id)
      sendJson $ write response

handlePost :: forall f next req res url
   . Category f
  => IsSymbol url
  => ReadForeign req
  => WriteForeign res
  => Config
  -> PostRoute req res url
  -> (req -> f next next -> CommandsF res)
  -> AppM _ Unit
handlePost config route command =
  E.post route handler'
  where
    route = reflectSymbol (SProxy :: SProxy url)
    handler' = do
      body <- getBody
      case runExcept (read body) of
        Right (r :: req) -> do
          response <- interpret config $ liftF (command r id)
          sendJson $ write response
        Left e -> do
          setStatus 400
          sendJson $ write {error: show e}

routes :: Config -> App _
routes config = do
  useExternal jsonBodyParser
  get files GetFiles
  get watched GetWatched
  post getIcons GetIcons
  post update UpdateWatched
  post open OpenFile
  post remove RemoveFile
  use $ static "dist"
  where
    get :: forall f next res url
       . Category f
      => IsSymbol url
      => WriteForeign res
      => GetRoute res url
      -> (f next next -> CommandsF res)
      -> AppM _ Unit
    get = handleGet config
    post :: forall f next req res url
       . Category f
      => IsSymbol url
      => ReadForeign req
      => WriteForeign res
      => PostRoute req res url
      -> (req -> f next next -> CommandsF res)
      -> AppM _ Unit
    post = handlePost config

main :: Eff _ Unit
main = void $ launchAff do
  dir' <- liftEff $ lookupEnv "FILETRACKER_DIR"
  case dir' of
    Nothing -> error "we done broke now!!!!"
    Just dir -> do
      db <- ensureDB $ concat [dir, "filetracker"]
      void $ liftEff $ listenHttp (routes {db, dir}) 3000 \_ ->
        log "Started server"
