module UI where

import Rockto.Config (appName)
import Rockto.Types
import Rockto.Utils

import qualified Data.List as L

import Control.Monad (forever, void)
import qualified Graphics.Vty as V

import Brick (Widget, str, withBorderStyle, (<+>), (<=>), emptyWidget, padTop, vBox, padRight, padLeft, padTop, padAll, Padding(..))
import qualified Brick.Widgets.Border.Style as BorderS
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)

drawUI :: GSt -> [ Widget () ]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str appName)
         (vBox[drawGame (st)
                      , padTop (Pad 2) $ drawGameOver (_dead st)
                      ]
           <+> vBorder
           <+> (drawScore (st)
           <=> hBorder
           <=> drawUsage (st)))
           
drawScore :: GSt -> Widget ()
drawScore st = 
    withBorderStyle BorderS.unicodeBold
          $ borderWithLabel (str "Score")
          $ center
          $ str $ "Round: " ++ (show . _round $ st)
                                ++ "\nTarget: " ++ (show . _target $ st)
                                ++ "\nPos: " ++ (show . _pos $ st)
                                ++ "\nDead: " ++ (show . _dead $ st)
                                ++ "\nDropping: " ++ (show . _droppingPositions $ st)
                                ++ "\nStable: " ++ (show . _stable $ st)
                                ++ "\nFinish: " ++ (show . _finish $ st)

drawUsage :: GSt -> Widget ()
drawUsage st =
    withBorderStyle BorderS.unicodeBold
    $ borderWithLabel (str "Usage")
    $ center
    $ str $  "\nMove:      ↑ ↓ ← →"
          ++ "\nRestart:   1 / r"
          ++ "\nQuit:      Esc / q"
          ++ "\nNew Game:  2 / s"
                      
drawGame :: GSt  -> Widget()
drawGame st =
  if _dead st
     then emptyWidget 
     else center (str $ L.intercalate "\n" (map unwords (getMapRepr st)))

drawGameOver :: Bool -> Widget()
drawGameOver dead =
  if dead
     then center (str "GAME OVER")
     else emptyWidget

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
