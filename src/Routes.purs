module Routes where

import Data.Foreign.Class (class Encode, class Decode)
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype)
import Types (FileData, OpenRequest, Path, Success, WatchedData)

newtype Route req res = Route
  { method :: Method
  , url :: String
  }

newtype Unused = Unused String
derive instance ntU :: Newtype Unused _
derive newtype instance isUU :: Decode Unused
derive newtype instance asUU :: Encode Unused

files :: Route Unused (Array Path)
files = Route {method: GET, url: "/api/files"}

watched :: Route Unused (Array WatchedData)
watched = Route {method: GET, url: "/api/watched"}

open :: Route OpenRequest Success
open = Route {method: POST, url: "/api/open"}

update :: Route FileData (Array WatchedData)
update = Route {method: POST, url: "/api/update"}
