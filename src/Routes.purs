module Routes where

import Prelude

import Data.HTTP.Method (Method(..))
import Types (FileData, GetIconsRequest, OpenRequest, Path, RemoveRequest, Operation, WatchedData)

foreign import kind RequestMethod
foreign import data GetRequest :: RequestMethod
foreign import data PostRequest :: RequestMethod

class GetHTTPMethod (method :: RequestMethod) where
  getHTTPMethod :: forall req res url
     . Route method req res url
    -> Method

instance ghmGR :: GetHTTPMethod GetRequest where
  getHTTPMethod _ = GET

instance ghmPR :: GetHTTPMethod PostRequest where
  getHTTPMethod _ = POST

data Route (method :: RequestMethod) req res (url :: Symbol) = Route

type GetRoute = Route GetRequest Void
type PostRoute = Route PostRequest

apiRoutes ::
  { files :: GetRoute (Array Path) "/api/files"
  , watched :: GetRoute (Array WatchedData) "/api/watched"
  , getIcons :: PostRoute GetIconsRequest Operation "/api/get-icons"
  , update :: PostRoute FileData (Array WatchedData) "/api/update"
  , open :: PostRoute OpenRequest Operation "/api/open"
  , remove :: PostRoute RemoveRequest Operation "/api/remove"
  }
apiRoutes =
  { files: Route
  , watched: Route
  , getIcons: Route
  , update: Route
  , open: Route
  , remove: Route
  }
