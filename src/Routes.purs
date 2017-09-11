module Routes where

import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Types (FileData, GetIconsRequest, OpenRequest, Path, RemoveRequest, Success, WatchedData)

newtype Route req res (url :: Symbol) = Route
  { method :: Method
  }

-- Unused params don't need to be decoded or encoded
type Unused = Foreign

files :: Route Unused (Array Path) "/api/files"
files = Route {method: GET}

watched :: Route Unused (Array WatchedData) "/api/watched"
watched = Route {method: GET}

open :: Route OpenRequest Success "/api/open"
open = Route {method: POST}

getIcons :: Route GetIconsRequest Success "/api/get-icons"
getIcons = Route {method: POST}

update :: Route FileData (Array WatchedData) "/api/update"
update = Route {method: POST}

remove :: Route RemoveRequest Success "/api/remove"
remove = Route {method: POST}
