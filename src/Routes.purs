module Routes where

import Data.Foreign.Class (class Encode, class Decode)
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype)
import Types (FileData, GetIconsRequest, OpenRequest, Path, RemoveRequest, Success, WatchedData)

newtype Route req res (url :: Symbol) = Route
  { method :: Method
  }

newtype Unused = Unused String
derive instance ntU :: Newtype Unused _
derive newtype instance isUU :: Decode Unused
derive newtype instance asUU :: Encode Unused

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
