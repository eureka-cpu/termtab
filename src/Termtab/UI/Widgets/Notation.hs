module Termtab.UI.Widgets.Notation (renderNotation) where

import Brick
import Termtab.Types (TrackIndex)
import Termtab.UI.Types (AppState (..), ResourceName)

renderNotation :: AppState -> TrackIndex -> Widget ResourceName
renderNotation _st _tIdx =
    vBox
        [ str staffLine
        , str staffLine
        , str staffLine
        , str staffLine
        , str staffLine
        , str "  (Standard notation: not yet implemented)"
        ]
  where
    staffLine = replicate 60 '-'
