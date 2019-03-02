module FrontEnd.Style where

import CSS

import CSS as CSS
import CSS.Common (auto, center)
import CSS.Flexbox (flexDirection)
import Data.NonEmpty as NE
import Halogen (ClassName(..))
import Prelude (discard, ($), (<>))

classNames ::
  { container :: ClassName
  , heatmap :: ClassName
  , filterWatched :: ClassName
  , refreshFiles :: ClassName
  , getIcons :: ClassName
  , file :: ClassName
  , watched :: ClassName
  , dot :: ClassName
  , filterLink :: ClassName
  , deleteLink :: ClassName
  , deleteConfirmation :: ClassName
  , fileEpisode :: ClassName
  , fileLink :: ClassName
  , fileButton :: ClassName
  , fileNote :: ClassName
  , top :: ClassName
  , topLeft :: ClassName
  , topRight :: ClassName
  }
classNames =
  { container: ClassName "container"
  , heatmap: ClassName "heatmap"
  , filterWatched: ClassName "filterWatched"
  , refreshFiles: ClassName "refreshFiles"
  , getIcons: ClassName "getIcons"
  , file: ClassName "file"
  , watched: ClassName "watched"
  , dot: ClassName "dot"
  , filterLink: ClassName "filterLink"
  , deleteLink: ClassName "deleteLink"
  , deleteConfirmation: ClassName "deleteConfirmation"
  , fileEpisode: ClassName "fileEpisode"
  , fileLink: ClassName "fileLink"
  , fileButton: ClassName "fileButton"
  , fileNote: ClassName "fileNote"
  , top: ClassName "top"
  , topLeft: ClassName "topLeft"
  , topRight: ClassName "topRight"
  }

class_ :: forall a. IsString a => ClassName -> a
class_ (ClassName cn) = CSS.fromString $ "." <> cn

stylesheet :: Rendered
stylesheet = render do
  class_ classNames.container ? do
    margin nil auto nil auto
    width (px 1024.0)
  class_ classNames.heatmap ? do
    width (pct 100.0)
    height (px 300.0)
  class_ classNames.filterWatched ? do
    marginTop (px 10.0)
    h4 ? do
      display inline
      marginRight (px 5.0)
  class_ classNames.refreshFiles ? do
    marginTop (px 10.0)
    display block
  class_ classNames.getIcons ? do
    marginTop (px 10.0)
    display block
  class_ classNames.file ? do
    display flex
    flexDirection row
    alignItems center
  class_ classNames.watched ? do
    backgroundColor (rgba 0 120 231 0.1)
  class_ classNames.dot ? do
    width (px 50.0)
    height (px 50.0)
    backgroundSize cover
    borderRadius (px 50.0) (px 50.0) (px 50.0) (px 50.0)
    backgroundPosition (positioned (pct 50.0) (pct 50.0))
    backgroundRepeat noRepeat
    star ? do
      position absolute
  class_ classNames.filterLink ? do
    width (px 100.0)
    opacity 0.25
  class_ classNames.filterLink `with` hover ? do
      opacity 1.0
  class_ classNames.deleteLink ? do
    boxSizing borderBox
    marginLeft (rem 1.0)
    width (px 100.0)
    opacity 0.25
  class_ classNames.deleteLink `with` hover ? do
      opacity 1.0
  class_ classNames.deleteConfirmation ? do
    backgroundColor red
    color white
  class_ classNames.fileEpisode ? do
    width (px 50.0)
  class_ classNames.fileLink ? do
    flexGrow 1
    flexShrink 1
    flexBasis nil
    padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
  class_ classNames.fileButton ? do
    width (px 200.0)
  class_ classNames.fileNote ? do
    width (px 150.0)
    margin (px 10.0) (px 10.0) (px 10.0) (px 10.0)
  body ? do
    fontFamily ["Noto Sans CJK JP"] $ NE.singleton sansSerif
  h3 ? do
    userSelect "none"
    cursor "pointer"
  a `with` hover ? do
    textDecoration underline
    cursor "pointer"
    backgroundColor (rgba 255 255 0 0.25)
  class_ classNames.top ? do
    marginTop (px 10.0)
    display flex
    flexDirection row
  class_ classNames.topLeft ? do
    flexGrow 1
    width (pct 50.0)
  class_ classNames.topRight ? do
    flexGrow 1
    width (pct 50.0)
  where
    opacity = key $ fromString "opacity"
    userSelect = key $ fromString "user-select"
    cursor = key $ fromString "cursor"
