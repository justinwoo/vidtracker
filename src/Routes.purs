module Routes where

import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.HTTP.Method (Method(..))
import Types (FileData, OpenRequest, Path, Success, WatchedData)

data Route req res = Route
  { method :: Method
  , url :: String
  }

newtype Unused = Unused String
derive newtype instance isUU :: IsForeign Unused
derive newtype instance asUU :: AsForeign Unused

files :: Route Unused (Array Path)
files = Route {method: GET, url: "/api/files"}

watched :: Route Unused (Array WatchedData)
watched = Route {method: GET, url: "/api/watched"}

open :: Route OpenRequest Success
open = Route {method: POST, url: "/api/open"}

update :: Route FileData (Array WatchedData)
update = Route {method: POST, url: "/api/update"}
