module Types where

import Prelude
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Foreign.Generic.Classes (class GenericDecode, class GenericEncode)
import Data.Foreign.Generic.Types (SumEncoding)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

options :: { unwrapSingleConstructors :: Boolean
, sumEncoding :: SumEncoding
, unwrapSingleArguments :: Boolean
}
options = defaultOptions {unwrapSingleConstructors = true}

readGeneric' :: forall a rep. (Generic a rep, GenericDecode rep) => Foreign -> F a
readGeneric' = readGeneric options

toForeignGeneric' :: forall a rep. (Generic a rep, GenericEncode rep) => a -> Foreign
toForeignGeneric' = toForeignGeneric options

newtype Path = Path String
derive instance eqPath :: Eq Path
derive instance ordPath :: Ord Path
derive instance ntPath :: Newtype Path _
derive newtype instance isPath :: IsForeign Path
derive newtype instance asPath :: AsForeign Path

newtype OpenRequest = OpenRequest
  { path :: Path
  }
derive instance grOR :: Generic OpenRequest _
instance ifOR :: IsForeign OpenRequest where
  read = readGeneric'
instance afOR :: AsForeign OpenRequest where
  write = toForeignGeneric'

newtype FileData = FileData
  { path :: Path
  , watched :: Boolean
  }
derive instance eqFD :: Eq FileData
derive instance ordFD :: Ord FileData
derive instance grFD :: Generic FileData _
instance ifFD :: IsForeign FileData where
  read = readGeneric'
instance afFD :: AsForeign FileData where
  write = toForeignGeneric'

newtype WatchedData = WatchedData
  { path :: Path
  , created :: String
  }
derive instance eqWD :: Eq WatchedData
derive instance ordWD :: Ord WatchedData
derive instance grWD :: Generic WatchedData _
instance ifWD :: IsForeign WatchedData where
  read = readGeneric'
instance afWD :: AsForeign WatchedData where
  write = toForeignGeneric'

newtype Success = Success
  { status :: String }
derive instance grSC :: Generic Success _
instance ifSC :: IsForeign Success where
  read = readGeneric'
instance afSC :: AsForeign Success where
  write = toForeignGeneric'