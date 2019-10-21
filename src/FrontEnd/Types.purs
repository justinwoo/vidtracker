module FrontEnd.Types where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Types (Path, WatchedData)

newtype DateString = DateString String
derive instance newtypeDateString :: Newtype DateString _

-- | our UI's main file mode
type File =
  { name :: Path
  , watched :: Maybe DateString
  , series :: Maybe String
  , episode :: Maybe Int
  , latest :: Maybe Int
  }

-- | the application state
type AppState =
  -- files
  { files :: Array File
  , filesLoading :: Boolean
  , paths :: Array Path
  , watchedData :: Array WatchedData
  , iconsLoading :: Boolean
  , grouped :: Boolean
  , filterWatched :: Boolean
  -- counted by positions from the top
  , cursor :: Maybe Int
  }

-- | Request actions for the requests driver
data Request
  = FetchFilesRequest
  | OpenFileRequest File
  | MarkFileRequest File
  | FetchIconsRequest

-- | responses from requests driver
type Response =
  { filesData :: Array Path
  , watchedData :: Array WatchedData
  }

-- | Actions for our UI element
data Action
  = FetchFiles
  | OpenFile
  | MarkFile
  | ToggleWatched Path
  | SetCursor Int
  | LinkClick Int File
  | WatchedClick Int File
  | EEQuery KeyboardEvent
  | Response Response

-- Direction for cursor movement
data Direction = Up | Down

-- | Events sourced from the keyboard driver
data KeyboardEvent
  -- move cursor on j/k
  = DirectionEvent Direction
  -- open file on o
  | OpenEvent
  -- mark file watched on m
  | MarkEvent
  -- call refresh files on r
  | RefreshEvent
  -- call fetch icons on I (shift + i)
  | FetchIconsEvent
  -- toggle grouping by show name
  | ToggleGroupedEvent
  -- toggle filtering watched items
  | ToggleFilterWatched

-- | Sometimes we will need to scroll to some item
data ScrollEvent
  = ScrollFileIntoView File

