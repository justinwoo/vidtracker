module Main where

import Prelude

import Bingsu ((<<>>))
import Bingsu as B
import Bonjiri (class NotJSPromise, PromiseSpec(..), mkPromiseSpec, resolve, run)
import Config as C
import Data.Array (filter, sortBy)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern), contains)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throwException)
import Effect.Exception as Ex
import Foreign (Foreign, unsafeToForeign)
import Global.Unsafe (unsafeStringify)
import Makkori as M
import Node.ChildProcess (Exit(..), defaultExecOptions, defaultSpawnOptions, exec, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Async as FS
import Node.FS.Stats (modifiedTime)
import Node.Path (concat)
import Node.Platform (Platform(..))
import Node.Process (exit, platform)
import Prim.Row as Row
import Prim.RowList (Cons, Nil, kind RowList)
import Record as Record
import Routes (GetRequest, PostRequest, Route, apiRoutes)
import SQLite3 (DBConnection, FilePath, newDB)
import SQLite3 as SQL
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeJSON)
import Simple.JSON as JSON
import Simple.JSON.Utils (printMultipleErrors)
import Sunde as Sunde
import Tortellini (parsellIni, printUhOhSpagghettios)
import Type.Prelude (class RowToList, RLProxy(RLProxy))
import Types (FileData, GetIconsRequest, OpenRequest, Operation, Path(Path), RemoveRequest, WatchedData)
import Unsafe.Coerce (unsafeCoerce)

data Error
  = ServerError String
  | UserError String
  | UnknownError String

writeError :: Error -> Foreign
writeError e = JSON.write
  case e of
    ServerError msg -> { tag: "ServerError", msg }
    UserError msg -> { tag: "UserError", msg }
    UnknownError msg -> { tag: "UnknownError", msg }

readError :: Foreign -> Error
readError f = do
  case JSON.read f of
    Right ({ tag, msg } :: { tag :: String, msg :: String }) ->
      case tag of
        "ServerError" -> ServerError msg
        "UserError" -> UserError msg
        _ -> UnknownError msg
    Left _ -> UnknownError $ unsafeStringify f

writeExError :: Ex.Error -> Foreign
writeExError = writeError <<< ServerError <<< Ex.message

prepareError :: Error -> {status :: Int, response :: String}
prepareError (UserError error) = {status: 400, response: writeJSON {error}}
prepareError (ServerError error) = {status: 500, response: writeJSON {error}}
prepareError (UnknownError _) = {status: 500, response: "Unknown error"}

getFiles :: Config -> PromiseSpec (Array Path)
getFiles { dir } = do
  filteredFiles <- filter (contains (Pattern "mkv")) <$> files
  sortAndUnassoc <$> traverse pairWithStat filteredFiles
  where
    files = mkPromiseSpec \res rej -> do
      FS.readdir dir case _ of
        Right xs -> res xs
        Left e -> rej (writeExError e)
    pairWithStat file = mkPromiseSpec \res rej -> do
      FS.stat (concat [dir, file]) case _ of
        Right s -> res (Tuple file s)
        Left e -> rej (writeError $ ServerError $ Ex.message e)
    sortByDate = sortBy <<< flip $ comparing (modifiedTime <<< snd)
    sortAndUnassoc tuples =
      Path <<< fst <$> sortByDate tuples

type Config =
  { db :: DBConnection
  , dir :: String
  , exe :: String
  }

readFail :: forall a. ReadForeign a => (String -> Error) -> Foreign -> PromiseSpec a
readFail ctr f = mkPromiseSpec \res rej ->
  case read f of
    Right x -> res x
    Left e -> rej $ writeError $ ctr $ printMultipleErrors e

runAff :: forall a. Aff a -> PromiseSpec a
runAff aff = mkPromiseSpec \res rej ->
  launchAff_ $ (liftEffect <<< res) =<< aff

runCallback :: forall a. NotJSPromise a => (FS.Callback a -> Effect Unit) -> PromiseSpec a
runCallback cb = mkPromiseSpec \res rej ->
  cb case _ of
    Right x -> res x
    Left e -> rej $ unsafeToForeign e

runCallback_ :: forall a. ((a -> Effect Unit) -> Effect Unit) -> PromiseSpec Unit
runCallback_ cb = mkPromiseSpec \res rej ->
  cb \_ -> res unit

runEffect :: forall a. Effect a -> PromiseSpec a
runEffect effect = PromiseSpec $ resolve =<< effect

getWatchedData :: Config -> PromiseSpec (Array WatchedData)
getWatchedData {db} = do
  results <- runAff do
    let query = B.literal "select path, created from watched order by created desc"
    B.queryDB db query {}
  readFail ServerError results

getIconsData :: Config -> GetIconsRequest -> PromiseSpec Operation
getIconsData {db} _ = do
  result <- runAff $ Sunde.spawn
    { cmd: "node"
    , args: ["get-icons.js"]
    , stdin: Nothing
    }
    defaultSpawnOptions
  pure $ case result.exit of
    Normally 0 -> {success: true}
    _ -> {success: false}

updateWatched :: Config -> FileData -> PromiseSpec (Array WatchedData)
updateWatched config@{db} ur = do
  let params = { path: unwrap ur.path }
  _ <- runAff $ if ur.watched
    then do
      let
        query
            = B.literal "insert or replace into watched ( path, created ) values ("
          <<>> B.param (B.Param :: _ "path" String)
          <<>> B.literal ", datetime() )"
      void $ B.queryDB db query params
    else do
      let
        queryString
             = B.literal "delete from watched where path = "
          <<>> B.param (B.Param :: _ "path" String)
      void $ B.queryDB db queryString params
  getWatchedData config

openFile :: Config -> OpenRequest -> PromiseSpec (Operation)
openFile {dir, exe} or = do
  let
    simpleOpen = case exe, platform of
      exe', _ | exe' /= "" -> Just exe'
      _, Just Linux -> Just "xdg-open"
      _, Just Darwin -> Just "open"
      _, _ -> Nothing
  _ <- runEffect $ case simpleOpen of
        Just command -> void $ spawn command (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
        _ -> void $ exec ("start \"\" \"rust-vlc-finder\" \"" <> concat [dir, unwrap or.path] <>  "\"") defaultExecOptions (const $ pure unit)
  pure {success: true}

removeFile :: Config -> RemoveRequest -> PromiseSpec (Operation)
removeFile {dir} rr = do
  let archive = concat [dir, "archive"]
  let name = unwrap rr.path
  let old = concat [dir, name]
  let new = concat [archive, name]
  runCallback_ $ FS.mkdir archive
  runCallback_ $ FS.rename old new
  pure $ {success: true}

ensureDB :: FilePath -> PromiseSpec DBConnection
ensureDB path = runAff do
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
         (req -> PromiseSpec res)
         where
  registerHandlerImpl route handler app =
    M.post (M.Path route') (M.makeHandler handler') app
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' req res = do
        body <- M.getBody req
        let
          onSuccess response = do
            M.sendResponse (JSON.writeJSON response) res
          onError e = do
            let { status, response } = prepareError $ readError e
            Console.error (unsafeCoerce e)
            M.setStatus status res
            M.sendResponse response res
        run
          onError
          onSuccess
          do
            r :: req <- readFail UserError body
            handler r

instance registerHandlerGet ::
  ( IsSymbol url
  , WriteForeign res
  ) => RegisterHandler
         (Route GetRequest Void res url)
         (PromiseSpec res)
         where
  registerHandlerImpl route handler app =
    M.get (M.Path route') (M.makeHandler handler') app
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' _ res = do
        let
          onSuccess response = do
            M.sendResponse (JSON.writeJSON response) res
          onError e = do
            let { status, response } = prepareError $ readError e
            Console.error (unsafeCoerce e)
            M.setStatus status res
            M.sendResponse response res
        run
          onError
          onSuccess
          handler

closeProgram :: Foreign -> Effect Unit
closeProgram e = do
  Console.error "Error on startup"
  Console.error (unsafeCoerce e)
  exit 1

startServer :: Config -> Effect Unit
startServer config = do
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

  void $ M.listen (M.Port 4567) (Console.log "Started server") app

main :: Effect Unit
main = run closeProgram startServer do
  configText <- runCallback $ FS.readTextFile UTF8 "./config.ini"

  vidtracker <- case parsellIni configText of
    Left e -> runEffect do
      throwException $
        Ex.error $ "Error parsing config.ini:\n" <> printUhOhSpagghettios e
    Right ({vidtracker: config} :: C.Config) -> pure config

  db <- ensureDB $ concat [vidtracker.dir, "filetracker"]
  pure $ Record.merge vidtracker {db}

foreign import accessControlMiddleware :: M.Middleware
