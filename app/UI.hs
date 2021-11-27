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
drawUI state = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str R.appName) $
         (center (str . show . getMap $ _map state)
           <+> vBorder
           <+> center (str $ "Round: " ++ (show . _round $ state)
                        ++ "\nScore: " ++ (show . _score $ state)
                        ++ "\nPos: " ++ (show . _pos $ state)
                      ))
