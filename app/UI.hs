module UI where

import qualified Rockto as R

import Control.Monad (void, forever)
import Graphics.Vty as Vty

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

rocktoApp :: Widget ()
rocktoApp =
    withBorderStyle unicode $
    borderWithLabel (str R.appName) $
    (center (str "Left") <+> vBorder <+> center (str "Right"))
