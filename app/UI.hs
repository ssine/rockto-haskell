{-# LANGUAGE OverloadedStrings #-}

module UI
  ( drawUI
  , uiAttrMap
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
--------------------------------------------------------------------------------
-- | Game UI Rendering
--------------------------------------------------------------------------------
drawUI :: GSt -> [Widget ()]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str appName)
         (vBox [drawGame st, padTop (Pad 2) $ drawGameOver (_dead st)]
          <=> hBorder
          <=> drawScore st)
-- Score Board
drawScore :: GSt -> Widget ()
drawScore st = withAttr scoreAttr (scboard st)

-- Game Cell
drawGame :: GSt  -> Widget()
drawGame st =
  if _dead st
    then emptyWidget
    else withBorderStyle BorderS.unicodeBold $ center $ vBox r
      where
        r = [hBox $ cells r | r <- [0..6]]
        cells y = [if a == x && b == y then showGrid TPlayer else showGrid (getMap (_map st) !! y !! x) | x <- [0..15]]
        (a, b) = _pos st

-- Game Over
drawGameOver :: Bool -> Widget()
drawGameOver dead =
  if dead
    then withAttr goAttr $ center wGameOver
    else emptyWidget

-- Wideget Style Setting
showGrid :: Tile -> Widget()
showGrid TExit     = withAttr exitAttr wExit
showGrid TWall     = withAttr wallAttr wWall
showGrid TScaffold = withAttr scaffoldAttr wScaffold
showGrid TParcel   = withAttr parcelAttr wParcel
showGrid TBrick    = withAttr brickAttr wBrick
showGrid TPlayer   = withAttr playerAttr wPlayer
showGrid TEmpty    = withAttr emptyAttr wEmpty

goAttr :: AttrName
goAttr = "over"

exitAttr, wallAttr, emptyAttr, scaffoldAttr, parcelAttr, brickAttr, playerAttr, scoreAttr :: AttrName
exitAttr  = "exit"
wallAttr = "wall"
scaffoldAttr = "scaffold"
parcelAttr = "parcel"
brickAttr = "brick"
emptyAttr = "empty"
playerAttr = "player"
scoreAttr = "score"

uiAttrMap :: AttrMap
uiAttrMap = attrMap V.defAttr
  [ (goAttr, fg V.red),
    (scoreAttr, fg V.green `V.withStyle` V.bold),
    (parcelAttr, fg V.green)
  ]

scboard    :: GSt -> Widget ()
scboard st = str $ "Round: " ++ (show . _round $ st) ++ "  Target: " ++ (show . _target $ st)
               ++ "                          Move:↑ ↓ ← →"
               ++ " Restart: 1 / r"
               ++ " Quit:Esc / q"
               ++ " New Game:2 / s"

wGameOver :: Widget ()
wGameOver = str $ "\n  █████▀██████████████████████████████████████████████"
               ++ "\n  █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─███─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█"
               ++ "\n  █─██▄─██─▀─███─█▄█─███─▄█▀███─██─██▄▀▄███─▄█▀██─▄─▄█"
               ++ "\n  ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀▀▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀"
               ++ "\n              Press 1 to restart                      "

wExit     :: Widget ()
wExit     = str $ "\n░░█▀▀▀▀▀█░░"
              ++  "\n░░█░░░░░█░░"
              ++  "\n░░█░░░█░█░░"
              ++  "\n░░█░░░░░█░░"
              ++  "\n░░█▄▄▄▄▄█░░"


wWall     :: Widget ()
wWall     = str $ "\n─────────█"
               ++ "\n█▄█▄█▄█▄█▐"
               ++ "\n███┼█████▐"
               ++ "\n█████████▐"

wParcel   :: Widget ()
wParcel   = str $ "\n░░█████╗░░"
               ++ "\n░██╔══██╗░"
               ++ "\n░███████║░"
               ++ "\n░██║░░██║░"
               ++ "\n░╚█████╔╝░"

wBrick    :: Widget ()
wBrick    = str $ "\n░░█████╗░░"
               ++ "\n░███████╗░"
               ++ "\n█████████░"
               ++ "\n░███████║░"
               ++ "\n░╚█████╔╝░"

wScaffold :: Widget ()
wScaffold = str $ "\n░░░░░░░░░░"
               ++ "\n░░░░░░░░░░"
               ++ "\n░░░░░░░░░░"
               ++ "\n░░░░░░░░░░"
               ++ "\n░░░░░░░░░░"

wPlayer   :: Widget ()
wPlayer   = str $ "\n    ██    "
               ++ "\n  ▄▀█▄▄▄  "
               ++ "\n▄▀  █▄▄   "
               ++ "\n ▄▄▄▀  ▀▄ "
               ++ "\n ▀       ▀"

wEmpty    :: Widget()
wEmpty    = str $ "\n          "
               ++ "\n          "
               ++ "\n          "
               ++ "\n          "
               ++ "\n          "
