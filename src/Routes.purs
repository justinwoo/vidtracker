module Routes where

import Prelude

import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))
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
apiRoutes = reflectRecordProxy

class ReflectRecordProxy a where
  reflectRecordProxy :: a

instance reflectRecordProxyInst ::
  ( RL.RowToList r rl
  , ReflectRecordProxyBuilder rl () r
  ) => ReflectRecordProxy { | r } where
  reflectRecordProxy = Builder.build builder {}
    where
      builder = reflectRecordProxyBuilder (RLProxy :: RLProxy rl)

class ReflectRecordProxyBuilder (rl :: RL.RowList) (i :: # Type) (o :: # Type)
  | rl -> i o where
  reflectRecordProxyBuilder :: RLProxy rl -> Builder { | i } { | o }

instance reflectRecordProxyBuilderNil :: ReflectRecordProxyBuilder RL.Nil () () where
  reflectRecordProxyBuilder _ = identity

instance reflectRecordProxyBuilderConsRoute ::
  ( ReflectRecordProxyBuilder tail from from'
  , Row.Lacks name from'
  , Row.Cons name (Route a b c d) from' to
  , IsSymbol name
  ) => ReflectRecordProxyBuilder (RL.Cons name (Route a b c d) tail) from to where
  reflectRecordProxyBuilder _ = first <<< rest
    where
      first = Builder.insert (SProxy :: SProxy name) Route
      rest = reflectRecordProxyBuilder (RLProxy :: RLProxy tail)
