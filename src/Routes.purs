module Routes where

import Prelude

import Naporitan as N
import Types (FileData, GetIconsRequest, OpenRequest, Path, RemoveRequest, Operation, WatchedData)

foreign import kind RequestMethod
foreign import data GetRequest :: RequestMethod
foreign import data PostRequest :: RequestMethod

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
apiRoutes = N.reflectRecordProxy

instance routeReflectProxy :: N.ReflectProxy (Route m i o u) where
  reflectProxy = Route
