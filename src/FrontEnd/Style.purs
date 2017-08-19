module FrontEnd.Style where

import CSS

import CSS.Common (auto, center)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (toLower)
import Halogen (ClassName(..))
import Prelude (class Show, discard, show, ($), (<>))

data ClassNames
  = Container
  | Heatmap
  | FilterWatched
  | RefreshFiles
  | GetIcons
  | File
  | Watched
  | Dot
  | FilterLink
  | DeleteLink
  | DeleteConfirmation
  | FileLink
  | FileButton
  | FileNote
derive instance gcn :: Generic ClassNames _
instance scn :: Show ClassNames where
  show = genericShow

container :: ClassName
container = ClassName (toLower $ show Container)
heatmap :: ClassName
heatmap = ClassName (toLower $ show Heatmap)
filterWatched :: ClassName
filterWatched = ClassName (toLower $ show FilterWatched)
refreshFiles :: ClassName
refreshFiles = ClassName (toLower $ show RefreshFiles)
getIcons :: ClassName
getIcons = ClassName (toLower $ show GetIcons)
file :: ClassName
file = ClassName (toLower $ show File)
watched :: ClassName
watched = ClassName (toLower $ show Watched)
dot :: ClassName
dot = ClassName (toLower $ show Dot)
filterLink :: ClassName
filterLink = ClassName (toLower $ show FilterLink)
deleteLink :: ClassName
deleteLink = ClassName (toLower $ show DeleteLink)
deleteConfirmation :: ClassName
deleteConfirmation = ClassName (toLower $ show DeleteConfirmation)
fileLink :: ClassName
fileLink = ClassName (toLower $ show FileLink)
fileButton :: ClassName
fileButton = ClassName (toLower $ show FileButton)
fileNote :: ClassName
fileNote = ClassName (toLower $ show FileNote)

class_ :: forall a. IsString a => ClassName -> a
class_ (ClassName cn) = fromString $ "." <> cn

stylesheet :: Rendered
stylesheet = render do
  class_ container ? do
    margin nil auto nil auto
    width (px 1024.0)
  class_ heatmap ? do
    width (pct 100.0)
    height (px 300.0)
  class_ filterWatched ? do
    marginTop (px 10.0)
    h4 ? do
      display inline
      marginRight (px 5.0)
  class_ refreshFiles ? do
    marginTop (px 10.0)
    display block
  class_ getIcons ? do
    marginTop (px 10.0)
    display block
  class_ file ? do
    display flex
    flexDirection row
    alignItems center
  class_ watched ? do
    backgroundColor (rgba 0 120 231 0.1)
  class_ dot ? do
    width (px 50.0)
    height (px 50.0)
    backgroundSize cover
    borderRadius (px 50.0) (px 50.0) (px 50.0) (px 50.0)
    backgroundPosition (positioned (pct 50.0) (pct 50.0))
    backgroundRepeat noRepeat
    star ? do
      position absolute
  class_ filterLink ? do
    width (px 100.0)
    opacity 0.25
  class_ filterLink `with` hover ? do
      opacity 1.0
  class_ deleteLink ? do
    boxSizing borderBox
    marginLeft (rem 1.0)
    width (px 100.0)
    opacity 0.25
  class_ deleteLink `with` hover ? do
      opacity 1.0
  class_ deleteConfirmation ? do
    backgroundColor red
    color white
  class_ fileLink ? do
    flexGrow 1
    flexShrink 1
    flexBasis nil
    padding (px 10.0) (px 10.0) (px 10.0) (px 10.0)
  class_ fileButton ? do
    width (px 200.0)
  class_ fileNote ? do
    width (px 150.0)
    margin (px 10.0) (px 10.0) (px 10.0) (px 10.0)
  h3 ? do
    userSelect "none"
    cursor "pointer"
  a `with` hover ? do
    textDecoration underline
    cursor "pointer"
    backgroundColor (rgba 255 255 0 0.25)
  where
    opacity = key $ fromString "opacity"
    userSelect = key $ fromString "user-select"
    cursor = key $ fromString "cursor"