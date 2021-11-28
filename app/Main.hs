{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rockto.Config (maxGameRound, tickDelayTimeNS)
import Rockto.Tick (tick)
import Rockto.Types (Direction (..), GSt (..))
import Rockto.Utils (mkInitS)
import UI (drawUI)

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


theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [("keyword1", fg V.magenta), ("keyword2", V.white `on` V.blue)]

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
