module UI where

import Rockto.Types
import qualified Rockto as R

import qualified Data.List as L

import Control.Monad (void, forever)
import qualified Graphics.Vty as V

import Brick (Widget, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

drawUI :: GSt -> [ Widget () ]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str R.appName) $
         (center (str
                  . L.intercalate "\n"
                  . map show
                  . getMap $ _map st)
           <+> vBorder
           <+> center (str $ "Round: " ++ (show . _round $ st)
                        ++ "\nScore: " ++ (show . _score $ st)
                        ++ "\nPos: " ++ (show . _pos $ st)
                      ))
