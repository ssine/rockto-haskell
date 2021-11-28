{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rockto.Config (callbackDelayTimeNS, maxGameRound)
import Rockto.Tick (tick)
import Rockto.Types (Direction (..), GSt (..))
import Rockto.Utils (mkInitS)
import UI (drawUI)

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import qualified Graphics.Vty as V
import qualified System.Random as R (newStdGen)

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), EventM, Next, attrMap, continue,
              defaultMain, fg, halt, neverShowCursor, on)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main (App (..), defaultMain, neverShowCursor)
import Brick.Util (fg, on)


theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [("keyword1", fg V.magenta), ("keyword2", V.white `on` V.blue)]

-- Run delayed callback when `stable` of the returned state is `False`
doTick :: Rockto.Types.Direction -> GSt -> EventM () (Next GSt)
doTick direction st =
  let nst = tick direction st
   in if _stable nst
        then continue nst
        else do
          liftIO (threadDelay callbackDelayTimeNS)
          doTick DNull nst

doTickNoCallback :: Rockto.Types.Direction -> GSt -> EventM () (Next GSt)
doTickNoCallback direction st = continue $ tick direction st

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

handleEvent :: GSt -> BrickEvent () e -> EventM () (Next GSt)
-- Quit
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt st
-- Reset
handleEvent st (VtyEvent (V.EvKey (V.KChar '1') [])) = resetCurrentGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'r') [])) = resetCurrentGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar '2') [])) = resetGame st
handleEvent st (VtyEvent (V.EvKey (V.KChar 's') [])) = resetGame st
-- Current round finished
handleEvent st@GSt{_finish=True} _ = newGame st
-- User input
handleEvent st (VtyEvent (V.EvKey key [])) =
  case key of
    V.KUp    -> doTick DUp st
    V.KDown  -> doTick DDown st
    V.KLeft  -> doTick DLeft st
    V.KRight -> doTick DRight st
    _        -> continue st
handleEvent st _ = continue st

app :: App GSt e ()
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
  n <- R.newStdGen
  void $ defaultMain app $ mkInitS n
