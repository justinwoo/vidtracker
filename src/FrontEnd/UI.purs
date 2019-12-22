module FrontEnd.UI where

import Prelude

import Calpis as C
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event as Event
import FrontEnd.HTTP (prefixUrl)
import FrontEnd.Types (Action(..), AppState, DateString(..), File)
import Global.Unsafe (unsafeEncodeURIComponent)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_) as Events
import React.Basic.Events (handler_) as Events
import Types (Path(..))

type Props =
  { appState :: AppState
  , push :: Action -> Effect Unit
  }

div' :: String -> Array RB.JSX -> RB.JSX
div' className children = R.div { className, children }

mkIconURL :: String -> String
mkIconURL series = "url(\"" <> un C.URL (prefixUrl $ iconsPath) <> "\")"
  where iconsPath = "/icons/" <> unsafeEncodeURIComponent series

_ui :: RB.Component Props
_ui = RB.createComponent "UI"

ui :: Props -> RB.JSX
ui = RB.makeStateless _ui \props ->
  let
    { appState: state } = props
    files = state.files

    header = div' "header"
      [ div' "info" $ [ R.h3_ [ R.text "Info:" ] ] <> infoLines
      , div' "recents" $ [ R.h3_ [ R.text "Recently watched:" ] ] <> recents
      ]

    infoLines = R.span_ <<< pure <<< R.text <$>
      [ "o: open current file"
      , "k: move cursor up"
      , "j: move cursor down"
      , "W/M: mark as watched"
      , "r: refresh"
      , "I: fetch icons and reload page"
      , "files loading: " <> if state.filesLoading then "true" else "false"
      , "icons loading: " <> if state.iconsLoading then "true" else "false"
      , "grouped by series: " <> if state.grouped then "true" else "false"
      , "filtering watched: " <> if state.filterWatched then "true" else "false"
      ]

    recents = mkRecent <$> Array.take (Array.length infoLines) state.watchedData

    mkRecent { path: Path name } = div' "recent" [ R.text name ]

    mkFile :: Int -> File -> RB.JSX
    mkFile idx file = RB.keyed (un Path file.name) $ fileElement { idx, file, props }

  in
    R.div_
      [ R.h1_ [R.text "vidtracker"]
      , header
      , R.div_ $ Array.mapWithIndex mkFile files
      ]

type FileProps =
  { idx :: Int
  , file :: File
  , props :: Props
  }

fileElement_ :: RB.Component FileProps
fileElement_ = RB.createComponent "FileElement"

fileElement :: FileProps -> RB.JSX
fileElement = RB.makeStateless fileElement_ \{ idx, file, props } ->
  let
    { appState: state, push } = props
  in
    R.div
      { className: intercalate " "
          [ "file"
          , case state.cursor of
              Just pos | pos == idx -> "cursor"
              _ -> ""
          , case file.watched of
              Just _ -> "done"
              Nothing -> ""
          ]
      , title : "latest watched: " <> fromMaybe "unknown" (show <$> file.latest)
      , onClick: Events.handler_ $ push (SetCursor idx)
      , children:
          [ R.div
              { className: "icon"
              , style:
                  case file.series of
                    Just series -> R.css { backgroundImage: mkIconURL series }
                    Nothing -> R.css {}
              }
          , R.div
              { className: "name"
              , title: (un Path file.name)
              , children: [ R.text $ un Path file.name ]
              , onClick: Events.capture_ $ push (LinkClick idx file)
              }
          , R.div
              { className: intercalate " "
                [ "watched"
                , maybe "" (const "has-date") file.watched
                ]
              , onClick: Events.capture_ $ push (WatchedClick idx file)
              , children:
                  [ R.text $ case file.watched of
                      Just (DateString date) -> "watched " <> date
                      Nothing -> maybe "" (\x -> " last: " <> show x) file.latest
                  ]
              }
        ]
    }

view :: Event AppState -> Effect (Event Action)
view appStates = do
  { event, push } <- Event.create

  _ <- Event.subscribe appStates \appState -> do
    renderJSX (ui { appState, push })

  pure event

foreign import renderJSX :: RB.JSX -> Effect Unit
