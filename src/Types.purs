module Types where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Path = Path String
derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path
derive instance ntPath :: Newtype Path _
derive newtype instance isPath :: ReadForeign Path
derive newtype instance asPath :: WriteForeign Path

type GetIconsRequest =
  {}

type RemoveRequest =
  { path :: Path
  }

type OpenRequest =
  { path :: Path
  }

type FileData =
  { path :: Path
  , watched :: Boolean
  }

type WatchedData =
  { path :: Path
  , created :: String
  }

type Operation =
  { success :: Boolean
  }
