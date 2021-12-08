module UI where
import Rockto.Config (appName)
import Rockto.Types
import Rockto.Utils

import qualified Data.List as L


import Control.Monad (forever, void)
import qualified Graphics.Vty as V

import Brick (App (..), Widget, AttrMap, attrMap, on, fg, str, hBox, withBorderStyle, (<+>), (<=>), emptyWidget, padTop, vBox, padRight, padLeft, padTop, padAll, Padding(..), attrMap, withAttr, AttrName)
import qualified Brick.Widgets.Border.Style as BorderS
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import qualified Data.Map as Map

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

drawGameOver :: Bool -> Widget()
drawGameOver dead =
  if dead   
    then center (str $   "\n  █████▀██████████████████████████████████████████████"
                    ++   "\n  █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─███─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█"
                    ++   "\n  █─██▄─██─▀─███─█▄█─███─▄█▀███─██─██▄▀▄███─▄█▀██─▄─▄█"
                    ++   "\n  ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀▀▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀"
     )
     else emptyWidget       

getMapSize :: Map.Map k v -> Int
getMapSize m = Map.size m

drawGame :: GSt  -> Widget()
drawGame g = 
  if _dead g
    then emptyWidget
  else
  withBorderStyle BorderS.unicodeBold
  $ center
  $ vBox r
  where
    r = [hBox $ cellsInRow r | r <- [0..6]]
    cellsInRow y = [if a == x && b == y then showGrid Lambda else drPos (getMap (_map g) !! y !! x) | x <- [0..15]]
    drPos    = showGrid . cellAt
    (a, b) = _pos g
    cellAt c
      | c == TExit  = Exit
      | c ==  TWall = Wall
      | c ==  TBrick = Brick
      | c ==  TParcel = Parcel
      | c ==  TScaffold = Scaffold
      | otherwise = Empty

data Cell = Exit | Wall | Scaffold | Parcel | Brick | Empty | Lambda
showGrid :: Cell -> Widget()
showGrid Exit = str " △ "
showGrid Wall  = str " ■ "
showGrid Scaffold  = str " □ "
showGrid Parcel  = str " ○ "
showGrid Brick  = str " ● "
showGrid Lambda  = str " λ "
showGrid Empty = emp


emp :: Widget()
emp = str "   "


