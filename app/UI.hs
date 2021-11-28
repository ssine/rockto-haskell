module UI where

import Rockto.Config (appName)
import Rockto.Types
import Rockto.Utils

import qualified Data.List as L

import Control.Monad (forever, void)
import qualified Graphics.Vty as V

import Brick (Widget, str, withBorderStyle, (<+>))
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)

drawUI :: GSt -> [ Widget () ]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str appName)
         (center (str $ L.intercalate "\n" (map show (getMapRepr st)))
           <+> vBorder
           <+> center (str $ "Round: " ++ (show . _round $ st)
                        ++ "\nScore: " ++ (show . _score $ st)
                        ++ "\nPos: " ++ (show . _pos $ st)
                        ++ "\nDead: " ++ (show . _dead $ st)
                        ++ "\nDropping: " ++ (show . _droppingPositions $ st)
                      ))

getMapRepr :: GSt  -> [[String]]
getMapRepr st = setList strList y $ setList (strList !! y) x "Player"
  where
    strList = map (map show) (getMap (_map st))
    (x, y) = _pos st
