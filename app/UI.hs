module UI where

import Rockto.Types
import qualified Rockto as R

import Control.Monad (void, forever)
import Graphics.Vty as Vty

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

drawUI :: GSt -> [ Widget () ]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str R.appName) $
         (center (str . show . getMap $ _map st)
           <+> vBorder
           <+> center (str $ "Round: " ++ (show . _round $ st)
                        ++ "\nScore: " ++ (show . _score $ st)
                        ++ "\nPos: " ++ (show . _pos $ st)
                      ))
