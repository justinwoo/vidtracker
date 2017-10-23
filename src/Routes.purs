module Routes where

import Prelude

import Data.HTTP.Method (Method(..))
import Types (FileData, GetIconsRequest, OpenRequest, Path, RemoveRequest, Success, WatchedData)

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

files :: GetRoute (Array Path) "/api/files"
files = Route

watched :: GetRoute (Array WatchedData) "/api/watched"
watched = Route

open :: PostRoute OpenRequest Success "/api/open"
open = Route

getIcons :: PostRoute GetIconsRequest Success "/api/get-icons"
getIcons = Route

update :: PostRoute FileData (Array WatchedData) "/api/update"
update = Route

remove :: PostRoute RemoveRequest Success "/api/remove"
remove = Route
