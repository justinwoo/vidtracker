module FrontEnd.Window where

import Prelude

import Effect (Effect)
import FRP.Event (Event)
import FRP.Event as Event
import FrontEnd.Types (Direction(..), KeyboardEvent(..), ScrollEvent(..))
import Types (Path)

foreign import addWindowKeyListener :: (String -> Effect Unit) -> Effect Unit
foreign import refreshPage :: Effect Unit
foreign import scrollIntoView :: Path -> Effect Unit
foreign import scrollToTop :: Effect Unit

window :: Event ScrollEvent -> Effect (Event KeyboardEvent)
window scrollEvent = do
  {event, push} <- Event.create
  addWindowKeyListener \key ->
    case key of
      "o" -> push OpenEvent
      "k" -> push $ DirectionEvent Up
      "j" -> push $ DirectionEvent Down
      "f" -> push ToggleFilterWatched
      "W" -> push MarkEvent
      "M" -> push MarkEvent
      "r" -> push RefreshEvent
      "I" -> push FetchIconsEvent
      "g" -> push ToggleGroupedEvent
      _ -> pure unit

  void $ Event.subscribe scrollEvent case _ of
    ScrollFileIntoView file ->
      scrollIntoView file.name

  pure event
