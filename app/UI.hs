{-# LANGUAGE OverloadedStrings #-}

module UI where

import Rockto.Config (appName)
import Rockto.Types (Direction (..), GSt (..), Tile (..), getMap)

import qualified Graphics.Vty as V

import Brick (App (..), AttrMap, AttrName, Padding (..), Widget, attrMap, emptyWidget, fg, hBox, on,
              padAll, padLeft, padRight, padTop, str, vBox, withAttr, withBorderStyle, (<+>), (<=>))
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Border.Style as BorderS
import Brick.Widgets.Center (center)


drawUI :: GSt -> [ Widget () ]
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
    cells y = [if a == x && b == y then showGrid Lambda else drPos (getMap (_map g) !! y !! x) | x <- [0..15]]
    drPos    = showGrid . cellAt
    (a, b) = _pos g
    cellAt n
      | n == TExit  = Exit
      | n ==  TWall = Wall
      | n ==  TBrick = Brick
      | n ==  TParcel = Parcel
      | n ==  TScaffold = Scaffold
      | otherwise = Empty

data Cell = Exit | Wall | Scaffold | Parcel | Brick | Empty | Lambda


drawGameOver :: Bool -> Widget()
drawGameOver dead =
  if dead
    then withAttr goAttr $center $ str $ "\n  █████▀██████████████████████████████████████████████"
                    ++   "\n  █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─███─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█"
                    ++   "\n  █─██▄─██─▀─███─█▄█─███─▄█▀███─██─██▄▀▄███─▄█▀██─▄─▄█"
                    ++   "\n  ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀▀▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀"
                    ++   "\n              Press 2 to restart                      "

     else emptyWidget

showGrid :: Cell -> Widget()
showGrid Exit     = withAttr exitAttr cw
showGrid Wall     = withAttr wallAttr wall
showGrid Scaffold = withAttr scaffoldAttr sc
showGrid Parcel   = withAttr parcelAttr parcel
showGrid Brick    = withAttr brickAttr br
showGrid Lambda   = withAttr lambdaAttr lam
showGrid Empty    = withAttr emptyAttr emp

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
