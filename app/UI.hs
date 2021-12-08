{-# LANGUAGE OverloadedStrings #-}

module UI
  ( drawUI
  , theMap
  )
where

import Rockto.Config (appName)
import Rockto.Types (GSt (..), Map (getMap), Tile (..))

import qualified Graphics.Vty as V

import Brick (Padding (Pad), Widget, fg)
import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Border.Style as BorderS
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (emptyWidget, hBox, padTop, str, vBox, withAttr, withBorderStyle, (<+>),
                           (<=>))

drawUI :: GSt -> [Widget ()]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str appName)
         (vBox[drawGame st
                      , padTop (Pad 2) $ drawGameOver (_dead st)
         ]
          <=> hBorder
          <=> drawScore st)

drawScore :: GSt -> Widget ()
drawScore st = str $ "Round: " ++ (show . _round $ st) ++ "  Target: " ++ (show . _target $ st)

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
drawGame g =
  if _dead g
    then emptyWidget
  else
  withBorderStyle BorderS.unicodeBold
  $ center
  $ vBox r
  where
    r = [hBox $ cells r | r <- [0..6]]
    cells y = [if a == x && b == y then showGrid TPlayer else drPos (getMap (_map g) !! y !! x) | x <- [0..15]]
    drPos    = showGrid
    (a, b) = _pos g


drawGameOver :: Bool -> Widget()
drawGameOver dead =
  if dead
    then withAttr goAttr $ center $ str $ "\n  █████▀██████████████████████████████████████████████"
                    ++   "\n  █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─███─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█"
                    ++   "\n  █─██▄─██─▀─███─█▄█─███─▄█▀███─██─██▄▀▄███─▄█▀██─▄─▄█"
                    ++   "\n  ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀▀▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀"
                    ++   "\n              Press 2 to restart                      "

    else emptyWidget

data Cell = Exit | Wall | Scaffold | Parcel | Brick | Empty | Lambda

showGrid :: Tile -> Widget()
showGrid TExit     = withAttr exitAttr cw
showGrid TWall     = withAttr wallAttr wall
showGrid TScaffold = withAttr scaffoldAttr sc
showGrid TParcel   = withAttr parcelAttr parcel
showGrid TBrick    = withAttr brickAttr br
showGrid TPlayer   = withAttr lambdaAttr lam
showGrid TEmpty    = withAttr emptyAttr emp

cw :: Widget ()
cw = str $ "\n░░░░░░░░░░"
        ++ "\n░░██████░░"
        ++ "\n░░█░░░░░░░"
        ++ "\n░░██████░░"
       ++  "\n░░░░░░░░░░"

wall :: Widget ()
wall = str $ "\n─────────█"
      ++   "\n█▄█▄█▄█▄█▐"
      ++   "\n███┼█████▐"
      ++   "\n█████████▐"

parcel :: Widget ()
parcel = str $ "\n░░█████╗░░"
          ++   "\n░██╔══██╗░"
          ++   "\n░███████║░"
          ++   "\n░██║░░██║░"
          ++   "\n░╚█████╔╝░"

br :: Widget ()
br = str $ "\n░░█████╗░░"
      ++   "\n░███████╗░"
      ++   "\n░███████║░"
      ++   "\n░███████║░"
      ++   "\n░╚█████╔╝░"

sc :: Widget ()
sc = str $ "\n░░░░░░░░░░"
        ++   "\n░░░░░░░░░░"
        ++   "\n░░░░░░░░░░"
        ++   "\n░░░░░░░░░░"
        ++   "\n░░░░░░░░░░"

lam :: Widget ()
lam = str $  "\n────██────"
        ++   "\n──▄▀█▄▄▄──"
        ++   "\n▄▀──█▄▄───"
        ++   "\n─▄▄▄▀──▀▄─"
        ++   "\n─▀───────▀"

emp :: Widget()
emp = str $  "\n          "
        ++   "\n          "
        ++   "\n          "
        ++   "\n          "
        ++   "\n          "

goAttr :: AttrName
goAttr = "over"

exitAttr, wallAttr, emptyAttr, scaffoldAttr, parcelAttr, brickAttr, lambdaAttr :: AttrName
exitAttr  = "exit"
wallAttr = "wall"
scaffoldAttr = "scaffold"
parcelAttr = "parcel"
brickAttr = "brick"
emptyAttr = "empty"
lambdaAttr = "lambda"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (goAttr, fg V.red)
  ]
