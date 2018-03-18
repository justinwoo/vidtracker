module Types where

import Prelude

import Chanpon.Classes (class FromResult, class ToParam)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Path = Path String
derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path
derive instance ntPath :: Newtype Path _
derive newtype instance isPath :: ReadForeign Path
derive newtype instance asPath :: WriteForeign Path
derive newtype instance tpPath :: ToParam Path
derive newtype instance fpPath :: FromResult Path

newtype GetIconsRequest = GetIconsRequest
  {}
derive newtype instance ifGIR :: ReadForeign GetIconsRequest
derive newtype instance afGIR :: WriteForeign GetIconsRequest

newtype RemoveRequest = RemoveRequest
  { path :: Path
  }
derive newtype instance ifRR :: ReadForeign RemoveRequest
derive newtype instance afRR :: WriteForeign RemoveRequest

newtype OpenRequest = OpenRequest
  { path :: Path
  }
derive newtype instance ifOR :: ReadForeign OpenRequest
derive newtype instance afOR :: WriteForeign OpenRequest

newtype FileData = FileData
  { path :: Path
  , watched :: Boolean
  }
derive newtype instance ifFD :: ReadForeign FileData
derive newtype instance afFD :: WriteForeign FileData

newtype WatchedData = WatchedData
  { path :: Path
  , created :: String
  }
derive instance eqWD :: Eq WatchedData
derive instance ordWD :: Ord WatchedData
derive newtype instance ifWD :: ReadForeign WatchedData
derive newtype instance afWD :: WriteForeign WatchedData

newtype Success = Success
  { status :: String
  }
derive newtype instance ifSC :: ReadForeign Success
derive newtype instance afSC :: WriteForeign Success
