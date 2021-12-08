{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rockto.Config (maxGameRound, tickDelayTimeNS)
import Rockto.Tick (tick)
import Rockto.Types (Direction (..), GSt (..))
import Rockto.Utils (mkInitS)

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import qualified Graphics.Vty as V
import qualified System.Random as R (newStdGen)

import Brick (App (..), AttrMap, BrickEvent (AppEvent, VtyEvent), EventM, Next, attrMap, continue,
              defaultMain, fg, halt, neverShowCursor, on)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App (..), customMain, defaultMain, neverShowCursor)
import Brick.Util (fg, on)
import Rockto.Config (appName)
import Rockto.Types
import Rockto.Utils

import qualified Data.List as L


import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color240 as R

import Brick (App (..), Widget, AttrMap, attrMap, on, fg, str, hBox, withBorderStyle, (<+>), (<=>), emptyWidget, padTop, vBox, padRight, padLeft, padTop, padAll, Padding(..), attrMap, withAttr, AttrName)
import qualified Brick.Widgets.Border.Style as BorderS
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import qualified Data.Map as Map
import qualified Graphics.Vty as R

drawUI :: GSt -> [ Widget () ]
drawUI st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str appName)
         (vBox[drawGame (st)
                      , padTop (Pad 2) $ drawGameOver (_dead st)
         ]
          <=> hBorder
          <=> (drawScore (st)))


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

drawGameOver :: Bool -> Widget()
drawGameOver dead =
  if dead   
    then withAttr goAttr $center $ str $ "\n  █████▀██████████████████████████████████████████████"
                    ++   "\n  █─▄▄▄▄██▀▄─██▄─▀█▀─▄█▄─▄▄─███─▄▄─█▄─█─▄█▄─▄▄─█▄─▄▄▀█"
                    ++   "\n  █─██▄─██─▀─███─█▄█─███─▄█▀███─██─██▄▀▄███─▄█▀██─▄─▄█"
                    ++   "\n  ▀▄▄▄▄▄▀▄▄▀▄▄▀▄▄▄▀▄▄▄▀▄▄▄▄▄▀▀▀▄▄▄▄▀▀▀▄▀▀▀▄▄▄▄▄▀▄▄▀▄▄▀"
                    ++   "\n              Press 2 to restart                      "

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
showGrid :: Cell -> Widget()
showGrid Exit = withAttr exitAttr cw
showGrid Wall  = withAttr wallAttr wall
showGrid Scaffold  = withAttr scaffoldAttr sc
showGrid Parcel  = withAttr parcelAttr parcel
showGrid Brick  = withAttr brickAttr br
showGrid Lambda  = withAttr lambdaAttr lam
showGrid Empty = withAttr emptyAttr emp

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

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (goAttr, fg V.red)
  ]

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


newGame :: GSt -> EventM () (Next GSt)
newGame st
  | gameRound == maxGameRound = continue st
  | otherwise = continue st{_round = gameRound + 1}
  where gameRound = _round st

resetCurrentGame :: GSt -> EventM () (Next GSt)
resetCurrentGame = continue . mkInitS . _seed

-- TODO: reset entire game
resetGame :: GSt -> EventM () (Next GSt)
resetGame = resetCurrentGame

data EvTick = EvTick

handleEvent :: GSt -> BrickEvent () EvTick -> EventM () (Next GSt)
handleEvent st@GSt{_stable=False} (AppEvent EvTick)   = continue $ tick DNull st
handleEvent st (AppEvent EvTick)                      = continue st
-- Quit
handleEvent st (VtyEvent (V.EvKey V.KEsc []))         = halt st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt st
-- Reset
handleEvent st (VtyEvent (V.EvKey (V.KChar '1') []))  = resetCurrentGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'r') []))  = resetCurrentGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar '2') []))  = resetGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar 's') []))  = resetGame st
-- Current round finished
handleEvent st@GSt{_finish=True} _ = newGame st
-- User input
handleEvent st@GSt{_stable=False} _ = continue st
handleEvent st (VtyEvent (V.EvKey key [])) =
  case key of
    V.KUp    -> continue $ tick DUp st
    V.KDown  -> continue $ tick DDown st
    V.KLeft  -> continue $ tick DLeft st
    V.KRight -> continue $ tick DRight st
    _        -> continue st
handleEvent st _ = continue st

app :: App GSt EvTick ()
app =
  App
    { appDraw = drawUI
    , appHandleEvent = handleEvent
    , appAttrMap = const theMap
    , appStartEvent = return
    , appChooseCursor = neverShowCursor
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan EvTick
    threadDelay tickDelayTimeNS
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  n <- R.newStdGen
  void $ customMain initialVty buildVty (Just chan) app $ mkInitS n
