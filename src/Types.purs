module Types where

import Prelude
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Foreign.Generic.Types (SumEncoding)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

options :: { unwrapSingleConstructors :: Boolean
, sumEncoding :: SumEncoding
, unwrapSingleArguments :: Boolean
}
options = defaultOptions {unwrapSingleConstructors = true}

genericDecode' :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> F a
genericDecode' = genericDecode options

genericEncode' :: forall a rep. Generic a rep => GenericEncode rep => a -> Foreign
genericEncode' = genericEncode options

newtype Path = Path String
derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path
derive instance ntPath :: Newtype Path _
derive newtype instance isPath :: Decode Path
derive newtype instance asPath :: Encode Path

data GetIconsRequest = GetIconsRequest
derive instance grGIR :: Generic GetIconsRequest _
instance ifGIR :: Decode GetIconsRequest where
  decode = genericDecode'
instance afGIR :: Encode GetIconsRequest where
  encode = genericEncode'

newtype OpenRequest = OpenRequest
  { path :: Path
  }
derive instance grOR :: Generic OpenRequest _
instance ifOR :: Decode OpenRequest where
  decode = genericDecode'
instance afOR :: Encode OpenRequest where
  encode = genericEncode'

newtype FileData = FileData
  { path :: Path
  , watched :: Boolean
  }
derive instance eqFD :: Eq FileData
derive instance ordFD :: Ord FileData
derive instance grFD :: Generic FileData _
instance ifFD :: Decode FileData where
  decode = genericDecode'
instance afFD :: Encode FileData where
  encode = genericEncode'

newtype WatchedData = WatchedData
  { path :: Path
  , created :: String
  }
derive instance eqWD :: Eq WatchedData
derive instance ordWD :: Ord WatchedData
derive instance grWD :: Generic WatchedData _
instance ifWD :: Decode WatchedData where
  decode = genericDecode'
instance afWD :: Encode WatchedData where
  encode = genericEncode'

newtype Success = Success
  { status :: String }
derive instance grSC :: Generic Success _
instance ifSC :: Decode Success where
  decode = genericDecode'
instance afSC :: Encode Success where
  encode = genericEncode'
