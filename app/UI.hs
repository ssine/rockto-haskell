module UI where

import Rockto.Config (appName)
import Rockto.Types
import Rockto.Utils

import qualified Data.List as L

import Control.Monad (forever, void)
import qualified Graphics.Vty as V

import Brick (Widget, str, withBorderStyle, (<+>), (<=>))
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)

drawUI :: GSt -> [ Widget () ]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str appName)
         (center (str $ L.intercalate "\n" (map unwords (getMapRepr st)))
           <+> vBorder
           <+> (center (str $ "Round: " ++ (show . _round $ st)
                        ++ "\nTarget: " ++ (show . _target $ st)
                        ++ "\nPos: " ++ (show . _pos $ st)
                        ++ "\nDead: " ++ (show . _dead $ st)
                        ++ "\nDropping: " ++ (show . _droppingPositions $ st)
                        ++ "\nStable: " ++ (show . _stable $ st)
                        ++ "\nFinish: " ++ (show . _finish $ st))
           <=> hBorder
           <=> center (str $ "  === USAGE ==="
                       ++ "\nMove:      ↑ ↓ ← →"
                       ++ "\nRestart:   1 / r"
                       ++ "\nQuit:      Esc / q"
                       ++ "\nNew Game:  2 / s"
                      )))

getMapRepr :: GSt  -> [[String]]
getMapRepr st = setList strList y $ setList (strList !! y) x "λ"
  where
    strList = map (map showGrid) (getMap (_map st))
    (x, y) = _pos st

showGrid :: Tile -> String
showGrid TBrick    = "●"
showGrid TParcel   = "○"
showGrid TWall     = "■"
showGrid TScaffold = "□"
showGrid TExit     = "△"
showGrid TEmpty    = " "
